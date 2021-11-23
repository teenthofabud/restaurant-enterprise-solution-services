package com.teenthofabud.restaurant.solution.customer.address.service.impl;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.fge.jsonpatch.JsonPatch;
import com.github.fge.jsonpatch.JsonPatchException;
import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.core.common.service.TOABBaseService;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountException;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountVo;
import com.teenthofabud.restaurant.solution.customer.account.service.AccountService;
import com.teenthofabud.restaurant.solution.customer.address.converter.AddressDto2EntityConverter;
import com.teenthofabud.restaurant.solution.customer.address.converter.AddressEntity2VoConverter;
import com.teenthofabud.restaurant.solution.customer.address.converter.AddressForm2EntityConverter;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressDto;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressEntity;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressException;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressForm;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressMessageTemplate;
import com.teenthofabud.restaurant.solution.customer.address.data.AddressVo;
import com.teenthofabud.restaurant.solution.customer.address.mapper.AddressEntitySelfMapper;
import com.teenthofabud.restaurant.solution.customer.address.mapper.AddressForm2EntityMapper;
import com.teenthofabud.restaurant.solution.customer.address.repository.AddressRepository;
import com.teenthofabud.restaurant.solution.customer.address.service.AddressService;
import com.teenthofabud.restaurant.solution.customer.address.validator.AddressDtoValidator;
import com.teenthofabud.restaurant.solution.customer.address.validator.AddressFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.customer.address.validator.AddressFormValidator;
import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Isolation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

@Component
@Slf4j
public class AddressServiceImpl implements AddressService {

    private static final Comparator<AddressVo> CMP_BY_NAME_AND_ACCOUNT_ID = (s1, s2) -> {
        return Integer.compare(s1.getAccountId().compareTo(s2.getAccountId()), s1.getName().compareTo(s2.getName()));
    };

    private AddressEntity2VoConverter entity2VoConverter;
    private AddressForm2EntityConverter form2EntityConverter;
    private AddressDto2EntityConverter dto2EntityConverter;
    private AddressForm2EntityMapper form2EntityMapper;
    private AddressEntitySelfMapper entitySelfMapper;
    private AddressFormValidator formValidator;
    private AddressFormRelaxedValidator relaxedFormValidator;
    private AddressDtoValidator dtoValidator;
    private AddressRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    private AccountService accountService;
    private Validator countryIdValidator;
    private Validator stateIdValidator;
    private Validator cityIdValidator;

    private String dobFormat;

    @Value("${res.customer.dob.format}")
    public void setDobFormat(String dobFormat) {
        this.dobFormat = dobFormat;
    }

    @Autowired
    public void setCountryIdValidator(Validator countryIdValidator) {
        this.countryIdValidator = countryIdValidator;
    }

    @Autowired
    public void setStateIdValidator(Validator stateIdValidator) {
        this.stateIdValidator = stateIdValidator;
    }

    @Autowired
    public void setCityIdValidator(Validator cityIdValidator) {
        this.cityIdValidator = cityIdValidator;
    }

    @Autowired
    public void setAccountService(AccountService accountService) {
        this.accountService = accountService;
    }

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setEntity2VoConverter(AddressEntity2VoConverter entity2VoConverter) {
        this.entity2VoConverter = entity2VoConverter;
    }

    @Autowired
    public void setDto2EntityConverter(AddressDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(AddressForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(AddressEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(AddressFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchAddressValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(AddressDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(AddressForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(AddressRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(AddressFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private List<AddressVo> entity2DetailedVoList(List<AddressEntity> addressEntityList) {
        List<AddressVo> addressDetailsList = new ArrayList<>(addressEntityList.size());
        for(AddressEntity entity : addressEntityList) {
            AddressVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            addressDetailsList.add(vo);
        }
        return addressDetailsList;
    }

    private Long parseAddressId(String id) throws AddressException {
        Long addressId = null;
        try {
            addressId = Long.parseLong(id);
            log.debug("Parsed id {} to address id {} in numeric format", id, addressId);
            if(addressId <= 0) {
                throw new NumberFormatException("address id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse address id", e);
            log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_ID_INVALID.getValue(), id);
            throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return addressId;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public Set<AddressVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all AddressEntity by their natural ordering");
        List<AddressEntity> addressEntityList = repository.findAll();
        Set<AddressVo> naturallyOrderedSet = new TreeSet<AddressVo>(CMP_BY_NAME_AND_ACCOUNT_ID);
        for(AddressEntity entity : addressEntityList) {
            AddressVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedSet.add(dto);
        }
        log.info("{} AddressVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public AddressVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws AddressException {
        log.info("Requesting AddressEntity by id: {}", id);
        Long addressId = parseAddressId(id);
        Optional<AddressEntity> optEntity = repository.findById(addressId);
        if(optEntity.isEmpty()) {
            log.debug("No AddressEntity found by id: {}", id);
            throw new AddressException(CustomerErrorCode.CUST_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found AddressVo by id: {}", id);
        AddressEntity entity = optEntity.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        AddressVo vo = entity2VoConverter.convert(entity);
        log.debug("AddressVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();

        return vo;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<AddressVo> retrieveAllMatchingDetailsByAccountId(String accountId) throws AddressException {
        log.info("Requesting AddressEntity that match with accountId: {}", accountId);
        Errors err = new DirectFieldBindingResult(accountId, "AddressForm");
        try {
            AccountVo accountVo = accountService.retrieveDetailsById(accountId, Optional.of(TOABCascadeLevel.TWO));
            if(!accountVo.getActive()) {
                throw new AccountException(CustomerErrorCode.CUST_INACTIVE, new Object [] { accountId });
            }
        } catch (AccountException e) {
            log.error("accountId is invalid", e);
            throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object [] { "accountId: " + accountId });
        }
        List<AddressEntity> addressEntityList = repository.findByAccountId(Long.parseLong(accountId));
        if(addressEntityList != null && !addressEntityList.isEmpty()) {
            List<AddressVo> matchedAddressList = entity2DetailedVoList(addressEntityList);
            log.info("Found {} AddressVo matching with accountId: {}", matchedAddressList.size(),accountId);
            return matchedAddressList;
        }
        log.debug("No AddressVo found matching with accountId: {}", accountId);
        throw new AddressException(CustomerErrorCode.CUST_NOT_FOUND, new Object[] { "accountId", accountId });
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<AddressVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName, Optional<String> optionalPincode,
                                                                Optional<String> optionalCityId, Optional<String> optionalStateId, Optional<String> optionalCountryId) throws AddressException {
        if(optionalName.isEmpty() && optionalPincode.isEmpty() && optionalCityId.isEmpty() && optionalStateId.isEmpty()
                && optionalCountryId.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String pincode = optionalPincode.isPresent() ? optionalPincode.get() : "";
        String cityId = optionalCityId.isPresent() ? optionalCityId.get() : "";
        String stateId = optionalStateId.isPresent() ? optionalStateId.get() : "";
        String countryId = optionalCountryId.isPresent() ? optionalCountryId.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(pincode))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(cityId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(stateId))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(countryId))) {
            log.debug("All search parameters are empty");
        }
        List<AddressVo> matchedAddressList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        AddressEntity entity = new AddressEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            log.debug("name {} is valid", name);
            providedFilters.put("name", name);
            entity.setName(name);
            matcherCriteria = matcherCriteria.withMatcher("name", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(pincode))) {
            log.debug("pincode {} is valid", pincode);
            providedFilters.put("pincode", pincode);
            entity.setPincode(pincode);
            matcherCriteria = matcherCriteria.withMatcher("pincode", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(cityId))) {
            Errors err = new DirectFieldBindingResult(cityId, "filters");
            cityIdValidator.validate(cityId, err);
            if(err.hasErrors()) {
                log.error("cityId is invalid: {}", err);
                log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_CITY_ID_INVALID.getValue(), cityId);
                throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "cityId", cityId });
            }
            log.debug("cityId {} is valid", cityId);
            providedFilters.put("cityId", cityId);
            entity.setCityId(cityId);
            matcherCriteria = matcherCriteria.withMatcher("cityId", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(stateId))) {
            Errors err = new DirectFieldBindingResult(stateId, "filters");
            stateIdValidator.validate(stateId, err);
            if(err.hasErrors()) {
                log.error("stateId is invalid: {}", err);
                log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_STATE_ID_INVALID.getValue(), stateId);
                throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "stateId", stateId });
            }
            log.debug("stateId {} is valid", stateId);
            providedFilters.put("stateId", stateId);
            entity.setStateId(stateId);
            matcherCriteria = matcherCriteria.withMatcher("stateId", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(countryId))) {
            Errors err = new DirectFieldBindingResult(countryId, "filters");
            countryIdValidator.validate(countryId, err);
            if(err.hasErrors()) {
                log.error("countryId is invalid: {}", err);
                log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_COUNTRY_ID_INVALID.getValue(), countryId);
                throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "countryId", countryId });
            }
            log.debug("countryId {} is valid", countryId);
            providedFilters.put("countryId", countryId);
            entity.setCountryId(countryId);
            matcherCriteria = matcherCriteria.withMatcher("countryId", match -> match.exact());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<AddressEntity> addressEntityExample = Example.of(entity, matcherCriteria);
        List<AddressEntity> addressEntityList = repository.findAll(addressEntityExample);
        if(addressEntityList != null && !addressEntityList.isEmpty()) {
            matchedAddressList = entity2DetailedVoList(addressEntityList);
            log.info("Found {} AddressVo matching with provided parameters : {}", matchedAddressList.size(), providedFilters);
        }
        log.info("No AddressVo available matching with provided parameters : {}", matchedAddressList.size(), providedFilters);
        return matchedAddressList;
    }

    @Override
    public List<AddressVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName, Optional<String> optionalAddressLine1,
                                                                Optional<String> optionalAddressLine2, Optional<String> optionalPincode,
                                                                Optional<String> optionalCityId, Optional<String> optionalStateId,
                                                                Optional<String> optionalCountryId) throws AddressException {
        if(optionalName.isEmpty() && optionalAddressLine1.isEmpty() && optionalAddressLine2.isEmpty()
                && optionalPincode.isEmpty() && optionalCityId.isEmpty()
                && optionalStateId.isEmpty() && optionalCountryId.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String addressLine1 = optionalAddressLine1.isPresent() ? optionalAddressLine1.get() : "";
        String addressLine2 = optionalAddressLine2.isPresent() ? optionalAddressLine2.get() : "";
        String pincode = optionalPincode.isPresent() ? optionalPincode.get() : "";
        String cityId = optionalCityId.isPresent() ? optionalCityId.get() : "";
        String stateId = optionalStateId.isPresent() ? optionalStateId.get() : "";
        String countryId = optionalCountryId.isPresent() ? optionalCountryId.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(addressLine1))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(addressLine2)) && StringUtils.isEmpty(StringUtils.trimWhitespace(pincode))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(cityId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(stateId))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(countryId))) {
            log.debug("All search parameters are empty");
        }
        List<AddressVo> matchedAddressList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        AddressEntity entity = new AddressEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            log.debug("name {} is valid", name);
            providedFilters.put("name", name);
            entity.setName(name);
            matcherCriteria = matcherCriteria.withMatcher("name", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(addressLine1))) {
            log.debug("addressLine1 {} is valid", addressLine1);
            providedFilters.put("addressLine1", addressLine1);
            entity.setAddressLine1(addressLine1);
            matcherCriteria = matcherCriteria.withMatcher("addressLine1", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(addressLine2))) {
            log.debug("addressLine2 {} is valid", addressLine2);
            providedFilters.put("addressLine2", addressLine2);
            entity.setAddressLine2(addressLine2);
            matcherCriteria = matcherCriteria.withMatcher("addressLine2", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(pincode))) {
            log.debug("pincode {} is valid", pincode);
            providedFilters.put("pincode", pincode);
            entity.setPincode(pincode);
            matcherCriteria = matcherCriteria.withMatcher("pincode", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(cityId))) {
            Errors err = new DirectFieldBindingResult(cityId, "filters");
            cityIdValidator.validate(cityId, err);
            if(err.hasErrors()) {
                log.error("cityId is invalid: {}", err);
                log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_CITY_ID_INVALID.getValue(), cityId);
                throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "cityId", cityId });
            }
            log.debug("cityId {} is valid", cityId);
            providedFilters.put("cityId", cityId);
            entity.setCityId(cityId);
            matcherCriteria = matcherCriteria.withMatcher("cityId", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(stateId))) {
            Errors err = new DirectFieldBindingResult(stateId, "filters");
            stateIdValidator.validate(stateId, err);
            if(err.hasErrors()) {
                log.error("stateId is invalid: {}", err);
                log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_STATE_ID_INVALID.getValue(), stateId);
                throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "stateId", stateId });
            }
            log.debug("stateId {} is valid", stateId);
            providedFilters.put("stateId", stateId);
            entity.setStateId(stateId);
            matcherCriteria = matcherCriteria.withMatcher("stateId", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(countryId))) {
            Errors err = new DirectFieldBindingResult(countryId, "filters");
            countryIdValidator.validate(countryId, err);
            if(err.hasErrors()) {
                log.error("countryId is invalid: {}", err);
                log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_COUNTRY_ID_INVALID.getValue(), countryId);
                throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "countryId", countryId });
            }
            log.debug("countryId {} is valid", countryId);
            providedFilters.put("countryId", countryId);
            entity.setCountryId(countryId);
            matcherCriteria = matcherCriteria.withMatcher("countryId", match -> match.exact());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<AddressEntity> addressEntityExample = Example.of(entity, matcherCriteria);
        List<AddressEntity> addressEntityList = repository.findAll(addressEntityExample);
        if(addressEntityList != null && !addressEntityList.isEmpty()) {
            matchedAddressList = entity2DetailedVoList(addressEntityList);
            log.info("Found {} AddressVo matching with provided parameters : {}", matchedAddressList.size(), providedFilters);
        }
        log.info("No AddressVo available matching with provided parameters : {}", matchedAddressList.size(), providedFilters);
        return matchedAddressList;
    }

    @Transactional
    @Override
    public String createAddress(AddressForm form) throws AddressException {
        log.info("Creating new AddressEntity");

        if(form == null) {
            log.debug("AddressForm provided is null");
            throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of AddressForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("AddressForm has {} errors", err.getErrorCount());
            CustomerErrorCode ec = CustomerErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("AddressForm error detail: {}", ec);
            throw new AddressException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of AddressForm are valid");

        log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_EXISTENCE_BY_NAME_AND_ACCOUNT_ID.getValue(), form.getName(), form.getAccountId());
        if(repository.existsByNameAndAccountId(form.getName(), Long.parseLong(form.getAccountId()))) {
            log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_EXISTS_BY_NAME_AND_ACCOUNT_ID.getValue(), form.getName(), form.getAccountId());
            throw new AddressException(CustomerErrorCode.CUST_EXISTS,
                    new Object[]{ "name: " + form.getName(), "accountId: " + form.getAccountId() });
        }
        log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_NON_EXISTENCE_BY_NAME_AND_ACCOUNT_ID.getValue(),
                form.getName(), form.getAccountId());

        AddressEntity expectedEntity = form2EntityConverter.convert(form);

        log.debug("Saving {}", expectedEntity);
        AddressEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new AddressException(CustomerErrorCode.CUST_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist AddressForm details" });
        }
        log.info("Created new AddressForm with id: {}", actualEntity.getId());
        return actualEntity.getId().toString();
    }

    @Transactional
    @Override
    public void updateAddress(String id, AddressForm form) throws AddressException {
        log.info("Updating AddressForm by id: {}", id);

        log.debug(AddressMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ADDRESS_ENTITY_ID.getValue(), id);
        Long addressId = parseAddressId(id);
        Optional<AddressEntity> optActualEntity = repository.findById(addressId);
        if(optActualEntity.isEmpty()) {
            log.debug(AddressMessageTemplate.MSG_TEMPLATE_NO_ADDRESS_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new AddressException(CustomerErrorCode.CUST_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(AddressMessageTemplate.MSG_TEMPLATE_FOUND_ADDRESS_ENTITY_ID.getValue(), id);

        AddressEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("AddressEntity is inactive with id: {}", id);
            throw new AddressException(CustomerErrorCode.CUST_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("AddressEntity is active with id: {}", id);

        if(form == null) {
            log.debug("AddressForm is null");
            throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of AddressForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("AddressForm has {} errors", err.getErrorCount());
            CustomerErrorCode ec = CustomerErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("AddressForm error detail: {}", ec);
            throw new AddressException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of AddressForm are empty");
            throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of AddressForm are valid");

        Optional<AddressEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of AddressForm");
            throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from AddressForm to AddressEntity");

        AddressEntity expectedEntity = optExpectedEntity.get();

        checkUniquenessOfAddress(form, actualEntity);

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from AddressEntity to AddressForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new AddressException(CustomerErrorCode.CUST_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency address details" });
        }
        log.info("Updated existing AddressEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void deleteAddress(String id) throws AddressException {
        log.info("Soft deleting AddressEntity by id: {}", id);

        log.debug(AddressMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ADDRESS_ENTITY_ID.getValue(), id);
        Long addressId = parseAddressId(id);
        Optional<AddressEntity> optEntity = repository.findById(addressId);
        if(optEntity.isEmpty()) {
            log.debug(AddressMessageTemplate.MSG_TEMPLATE_NO_ADDRESS_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new AddressException(CustomerErrorCode.CUST_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(AddressMessageTemplate.MSG_TEMPLATE_FOUND_ADDRESS_ENTITY_ID.getValue(), id);

        AddressEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("AddressEntity is inactive with id: {}", id);
            throw new AddressException(CustomerErrorCode.CUST_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("AddressEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        AddressEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new AddressException(CustomerErrorCode.CUST_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current address details with id:" + id });
        }

        log.info("Soft deleted existing AddressEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void applyPatchOnAddress(String id, List<PatchOperationForm> patches) throws AddressException {
        log.info("Patching AddressEntity by id: {}", id);

        log.debug(AddressMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ADDRESS_ENTITY_ID.getValue(), id);
        Long addressId = parseAddressId(id);
        Optional<AddressEntity> optActualEntity = repository.findById(addressId);
        if(optActualEntity.isEmpty()) {
            log.debug(AddressMessageTemplate.MSG_TEMPLATE_NO_ADDRESS_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new AddressException(CustomerErrorCode.CUST_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(AddressMessageTemplate.MSG_TEMPLATE_FOUND_ADDRESS_ENTITY_ID.getValue(), id);

        AddressEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Address patch list not provided");
            throw new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Address patch list has {} items", patches.size());


        log.debug("Validating patch list items for Address");
        try {
            toabBaseService.validatePatches(patches, CustomerErrorCode.CUST_EXISTS.getDomain() + ":LOV");
            log.debug("All Address patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Address patch item are invalid");
            throw new AddressException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Address");


        log.debug("Patching list items to AddressDto");
        AddressDto patchedAddressForm = new AddressDto();
        try {
            log.debug("Preparing patch list items for Address");
            JsonNode addressDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch addressPatch = JsonPatch.fromJson(addressDtoTree);
            log.debug("Prepared patch list items for Address");
            JsonNode blankAddressDtoTree = om.convertValue(new AddressDto(), JsonNode.class);
            JsonNode patchedAddressFormTree = addressPatch.apply(blankAddressDtoTree);
            log.debug("Applying patch list items to AddressDto");
            patchedAddressForm = om.treeToValue(patchedAddressFormTree, AddressDto.class);
            log.debug("Applied patch list items to AddressDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to AddressDto: {}", e);
            AddressException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in AddressDto");
                ex = new AddressException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new AddressException(CustomerErrorCode.CUST_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to AddressDto: {}", e);
            throw new AddressException(CustomerErrorCode.CUST_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to AddressDto");

        log.debug("Validating patched AddressDto");
        Errors err = new DirectFieldBindingResult(patchedAddressForm, patchedAddressForm.getClass().getSimpleName());
        dtoValidator.validate(patchedAddressForm, err);
        if(err.hasErrors()) {
            log.debug("Patched AddressDto has {} errors", err.getErrorCount());
            CustomerErrorCode ec = CustomerErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched AddressDto error detail: {}", ec);
            throw new AddressException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched AddressDto are valid");

        checkUniquenessOfAddress(patchedAddressForm, actualEntity);

        log.debug("Comparatively copying patched attributes from AddressDto to AddressEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedAddressForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (AddressException) e;
        }
        log.debug("Comparatively copied patched attributes from AddressDto to AddressEntity");

        log.debug("Saving patched AddressEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched AddressEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete AddressEntity with id:{}", id);
            throw new AddressException(CustomerErrorCode.CUST_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency address details with id:" + id });
        }
        log.info("Patched AddressEntity with id:{}", id);
    }

    private void checkUniquenessOfAddress(AddressDto patchedAddressForm, AddressEntity actualEntity) throws AddressException {
        // name = true, accountId = false
        if(patchedAddressForm.getName().isPresent() && patchedAddressForm.getAccountId().isEmpty()) {
            log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_EXISTENCE_BY_NAME_AND_ACCOUNT_ID.getValue(),
                    patchedAddressForm.getName().get(), actualEntity.getAccount().getId());
            boolean sameEntitySw = patchedAddressForm.getName().get().compareTo(actualEntity.getName()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndAccountId(patchedAddressForm.getName().get(), actualEntity.getAccount().getId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_EXISTS_BY_NAME_AND_ACCOUNT_ID.getValue(),
                        patchedAddressForm.getName().get(), actualEntity.getAccount().getId());
                throw new AddressException(CustomerErrorCode.CUST_EXISTS,
                        new Object[]{ "name: " + patchedAddressForm.getName().get(), ", accountId: " + actualEntity.getAccount().getId() });
            }
            log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_NON_EXISTENCE_BY_NAME_AND_ACCOUNT_ID.getValue(),
                    patchedAddressForm.getName().get(), actualEntity.getAccount().getId());
        }

        // name = true, accountId = true
        if(patchedAddressForm.getName().isPresent() && patchedAddressForm.getAccountId().isPresent()) {
            log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_EXISTENCE_BY_NAME_AND_ACCOUNT_ID.getValue(),
                    patchedAddressForm.getName().get(), patchedAddressForm.getAccountId().get());
            boolean sameEntitySw = patchedAddressForm.getName().get().compareTo(actualEntity.getName().toString()) == 0
                    && patchedAddressForm.getAccountId().get().compareTo(actualEntity.getAccount().getId().toString()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndAccountId(patchedAddressForm.getName().get(),
                    Long.parseLong(patchedAddressForm.getAccountId().get()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_EXISTS_BY_NAME_AND_ACCOUNT_ID.getValue(),
                        patchedAddressForm.getName().get(), patchedAddressForm.getAccountId().get());
                throw new AddressException(CustomerErrorCode.CUST_EXISTS,
                        new Object[]{ "name: " + patchedAddressForm.getName().get(), ", accountId: " + patchedAddressForm.getAccountId().get() });
            }
            log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_NON_EXISTENCE_BY_NAME_AND_ACCOUNT_ID.getValue(),
                    patchedAddressForm.getName().get(), patchedAddressForm.getAccountId().get());
        }

        // name = false, accountId = true
        if(patchedAddressForm.getName().isEmpty() && patchedAddressForm.getAccountId().isPresent()) {
            log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_EXISTENCE_BY_NAME_AND_ACCOUNT_ID.getValue(),
                    actualEntity.getName(),  patchedAddressForm.getAccountId().get());
            boolean sameEntitySw = patchedAddressForm.getAccountId().get().compareTo(actualEntity.getAccount().getId().toString()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndAccountId(
                    actualEntity.getName(), Long.parseLong(patchedAddressForm.getAccountId().get()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_EXISTS_BY_NAME_AND_ACCOUNT_ID.getValue(),
                        actualEntity.getName(), patchedAddressForm.getAccountId().get());
                throw new AddressException(CustomerErrorCode.CUST_EXISTS, new Object[]{ "name " + actualEntity.getName(),
                        ", accountId: " + patchedAddressForm.getAccountId().get() });
            }
            log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_NON_EXISTENCE_BY_NAME_AND_ACCOUNT_ID.getValue(),
                    actualEntity.getName(),  patchedAddressForm.getAccountId().get());
        }

    }

    private void checkUniquenessOfAddress(AddressForm addressForm, AddressEntity actualEntity) throws AddressException {
        // name = true, accountId = false
        if(StringUtils.hasText(StringUtils.trimWhitespace(addressForm.getName()))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(addressForm.getAccountId()))) {
            log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_EXISTENCE_BY_NAME_AND_ACCOUNT_ID.getValue(),
                    addressForm.getName(), actualEntity.getAccount().getId());
            boolean sameEntitySw = addressForm.getName().compareTo(actualEntity.getName()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndAccountId(
                    addressForm.getName(), actualEntity.getAccount().getId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_EXISTS_BY_NAME_AND_ACCOUNT_ID.getValue(),
                        addressForm.getName(), actualEntity.getAccount().getId());
                throw new AddressException(CustomerErrorCode.CUST_EXISTS,
                        new Object[]{ "name: " + addressForm.getName(), ", accountId: " + actualEntity.getAccount().getId() });
            }
            log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_NON_EXISTENCE_BY_NAME_AND_ACCOUNT_ID.getValue(),
                    addressForm.getName(), actualEntity.getAccount().getId());
        }

        // name = true, accountId = true
        if(StringUtils.hasText(StringUtils.trimWhitespace(addressForm.getName()))
                && StringUtils.hasText(StringUtils.trimWhitespace(addressForm.getAccountId()))) {
            log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_EXISTENCE_BY_NAME_AND_ACCOUNT_ID.getValue(),
                    addressForm.getName(), addressForm.getAccountId());
            boolean sameEntitySw = addressForm.getName().compareTo(actualEntity.getName()) == 0
                    && addressForm.getAccountId().compareTo(actualEntity.getAccount().getId().toString()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndAccountId(addressForm.getName(), Long.parseLong(addressForm.getAccountId()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_EXISTS_BY_NAME_AND_ACCOUNT_ID.getValue(),
                        addressForm.getName(), addressForm.getAccountId());
                throw new AddressException(CustomerErrorCode.CUST_EXISTS,
                        new Object[]{ "accountId: " + addressForm.getAccountId(), ", name: " + addressForm.getName() });
            }
            log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_NON_EXISTENCE_BY_NAME_AND_ACCOUNT_ID.getValue(),
                    addressForm.getName(), addressForm.getAccountId());
        }

        // name = false, accountId = true
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(addressForm.getName()))
                && StringUtils.hasText(StringUtils.trimWhitespace(addressForm.getAccountId()))) {
            log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_EXISTENCE_BY_NAME_AND_ACCOUNT_ID.getValue(),
                    actualEntity.getName(),  addressForm.getAccountId());
            boolean sameEntitySw = addressForm.getName().compareTo(actualEntity.getName()) == 0
                    && addressForm.getAccountId().compareTo(actualEntity.getAccount().getId().toString()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndAccountId(actualEntity.getName(), Long.parseLong(addressForm.getAccountId()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_EXISTS_BY_NAME_AND_ACCOUNT_ID.getValue(),
                        actualEntity.getName(),  addressForm.getAccountId());
                throw new AddressException(CustomerErrorCode.CUST_EXISTS, new Object[]{ "name: " + actualEntity.getName(),
                        ", accountId: " + addressForm.getAccountId() });
            }
            log.debug(AddressMessageTemplate.MSG_TEMPLATE_ADDRESS_NON_EXISTENCE_BY_NAME_AND_ACCOUNT_ID.getValue(),
                    actualEntity.getName(),  addressForm.getAccountId());
        }

    }

}