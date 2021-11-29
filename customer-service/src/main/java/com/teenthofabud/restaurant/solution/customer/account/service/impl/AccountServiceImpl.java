package com.teenthofabud.restaurant.solution.customer.account.service.impl;

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
import com.teenthofabud.restaurant.solution.customer.account.converter.AccountDto2EntityConverter;
import com.teenthofabud.restaurant.solution.customer.account.converter.AccountForm2EntityConverter;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountDto;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountEntity;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountException;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountForm;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountMessageTemplate;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountVo;
import com.teenthofabud.restaurant.solution.customer.account.mapper.AccountEntitySelfMapper;
import com.teenthofabud.restaurant.solution.customer.account.mapper.AccountForm2EntityMapper;
import com.teenthofabud.restaurant.solution.customer.account.repository.AccountRepository;
import com.teenthofabud.restaurant.solution.customer.account.service.AccountService;
import com.teenthofabud.restaurant.solution.customer.utils.CustomerServiceHelper;
import com.teenthofabud.restaurant.solution.customer.account.validator.AccountDtoValidator;
import com.teenthofabud.restaurant.solution.customer.account.validator.AccountFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.customer.account.validator.AccountFormValidator;
import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
import com.teenthofabud.restaurant.solution.customer.integration.metadata.gender.validator.GenderIdValidator;
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

import java.io.IOException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
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
public class AccountServiceImpl implements AccountService {

    private static final Comparator<AccountVo> CMP_BY_FIRST_NAME_AND_LAST_NAME = (s1, s2) -> {
        return Integer.compare(s1.getFirstName().compareTo(s2.getFirstName()), s1.getLastName().compareTo(s2.getLastName()));
    };

    private AccountForm2EntityConverter form2EntityConverter;
    private AccountDto2EntityConverter dto2EntityConverter;
    private AccountForm2EntityMapper form2EntityMapper;
    private AccountEntitySelfMapper entitySelfMapper;
    private AccountFormValidator formValidator;
    private AccountFormRelaxedValidator relaxedFormValidator;
    private AccountDtoValidator dtoValidator;
    private AccountRepository repository;
    private CustomerServiceHelper customerServiceHelper;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    private GenderIdValidator genderIdValidator;

    private String dobFormat;

    @Value("${res.customer.dob.format}")
    public void setDobFormat(String dobFormat) {
        this.dobFormat = dobFormat;
    }

    @Autowired
    public void setGenderIdValidator(GenderIdValidator genderIdValidator) {
        this.genderIdValidator = genderIdValidator;
    }

    @Autowired
    public void setCustomerServiceHelper(CustomerServiceHelper customerServiceHelper) {
        this.customerServiceHelper = customerServiceHelper;
    }

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setDto2EntityConverter(AccountDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(AccountForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(AccountEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(AccountFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchAccountValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(AccountDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(AccountForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(AccountRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(AccountFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private Long parseAccountId(String id) throws AccountException {
        Long accountId = null;
        try {
            accountId = Long.parseLong(id);
            log.debug("Parsed id {} to account id {} in numeric format", id, accountId);
            if(accountId <= 0) {
                throw new NumberFormatException("account id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse account id", e);
            log.debug(AccountMessageTemplate.MSG_TEMPLATE_ACCOUNT_ID_INVALID.getValue(), id);
            throw new AccountException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return accountId;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public Set<AccountVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all AccountEntity by their natural ordering");
        List<AccountEntity> accountEntityList = repository.findAll();
        List<AccountVo> accountVoList = customerServiceHelper.accountEntity2DetailedVo(accountEntityList);
        Set<AccountVo> naturallyOrderedSet = new TreeSet<AccountVo>(CMP_BY_FIRST_NAME_AND_LAST_NAME);
        naturallyOrderedSet.addAll(accountVoList);
        log.info("{} AccountVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public AccountVo retrieveDetailsById(String id) throws AccountException {
        log.info("Requesting AccountEntity by id: {}", id);
        Long accountId = parseAccountId(id);
        Optional<AccountEntity> optEntity = repository.findById(accountId);
        if(optEntity.isEmpty()) {
            log.debug("No AccountEntity found by id: {}", id);
            throw new AccountException(CustomerErrorCode.CUST_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        AccountEntity entity = optEntity.get();
        AccountVo vo = customerServiceHelper.accountEntity2DetailedVo(entity);
        log.info("Found AccountVo by id: {}", id);
        return vo;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public AccountVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws AccountException {
        log.info("Requesting AccountEntity by id: {}", id);
        Long accountId = parseAccountId(id);
        Optional<AccountEntity> optEntity = repository.findById(accountId);
        if(optEntity.isEmpty()) {
            log.debug("No AccountEntity found by id: {}", id);
            throw new AccountException(CustomerErrorCode.CUST_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found AccountVo by id: {}", id);
        AccountEntity entity = optEntity.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        AccountVo vo = customerServiceHelper.accountEntity2DetailedVo(entity);
        log.debug("AccountVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<AccountVo> retrieveAllMatchingDetailsByGenderId(String genderId) throws AccountException {
        log.info("Requesting AccountEntity that match with genderId: {}", genderId);
        Errors err = new DirectFieldBindingResult(genderId, "AccountForm");
        genderIdValidator.validate(genderId, err);
        if(err.hasErrors()) {
            CustomerErrorCode ec = CustomerErrorCode.valueOf(err.getGlobalError().getCode());
            log.debug("genderId error detail: {}", ec);
            log.error("genderId is invalid: {}", ec);
            throw new AccountException(ec, new Object [] { "genderId: " + genderId });
        }
        List<AccountEntity> accountEntityList = repository.findByGenderId(genderId);
        List<AccountVo> matchedAccountList = customerServiceHelper.accountEntity2DetailedVo(accountEntityList);
        log.info("Found {} AccountVo matching with genderId: {}", matchedAccountList.size(),genderId);
        if(matchedAccountList != null && !matchedAccountList.isEmpty()) {
            return matchedAccountList;
        }
        log.debug("No AccountVo found matching with genderId: {}", genderId);
        throw new AccountException(CustomerErrorCode.CUST_NOT_FOUND, new Object[] { "genderId", genderId });
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<AccountVo> retrieveAllMatchingDetailsByFirstNameLastNameDateOfBirth(Optional<String> optionalFirstName, Optional<String> optionalLastName, Optional<String> optionalDateOfBirth) throws AccountException {
        if(optionalFirstName.isEmpty() && optionalLastName.isEmpty() && optionalDateOfBirth.isEmpty()) {
            log.debug("All search parameters are empty");
        }
        String firstName = optionalFirstName.isPresent() ? optionalFirstName.get() : "";
        String lastName = optionalLastName.isPresent() ? optionalLastName.get() : "";
        String dateOfBirth = optionalDateOfBirth.isPresent() ? optionalDateOfBirth.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(firstName)) && StringUtils.isEmpty(StringUtils.trimWhitespace(lastName))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(dateOfBirth))) {
            log.debug("All search parameters are empty");
        }
        List<AccountVo> matchedAccountList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        AccountEntity entity = new AccountEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(firstName))) {
            log.debug("firstName {} is valid", firstName);
            providedFilters.put("firstName", firstName);
            entity.setFirstName(firstName);
            matcherCriteria = matcherCriteria.withMatcher("firstName", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(lastName))) {
            log.debug("lastName {} is valid", lastName);
            providedFilters.put("lastName", lastName);
            entity.setLastName(lastName);
            matcherCriteria = matcherCriteria.withMatcher("lastName", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(dateOfBirth))) {
            try {
                DateTimeFormatter dtf = DateTimeFormatter.ofPattern(dobFormat);
                LocalDate dob = LocalDate.parse(dateOfBirth, dtf);
                log.debug("dateOfBirth {} is valid", dateOfBirth);
                providedFilters.put("dateOfBirth", dateOfBirth);
                entity.setDateOfBirth(dob);
                matcherCriteria = matcherCriteria.withMatcher("dateOfBirth", match -> match.exact());
            } catch (DateTimeParseException e) {
                log.error("Unable to parse dateOfBirth", e);
                log.debug(AccountMessageTemplate.MSG_TEMPLATE_ACCOUNT_DOB_INVALID.getValue(), dateOfBirth);
                throw new AccountException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "dateOfBirth", dateOfBirth });
            }
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<AccountEntity> accountEntityExample = Example.of(entity, matcherCriteria);
        List<AccountEntity> accountEntityList = repository.findAll(accountEntityExample);
        matchedAccountList = customerServiceHelper.accountEntity2DetailedVo(accountEntityList);
        log.info("Found {} AccountVo matching with provided parameters : {}", matchedAccountList.size(), providedFilters);
        return matchedAccountList;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<AccountVo> retrieveAllMatchingDetailsByCriteria(
            Optional<String> optionalFirstName, Optional<String> optionalLastName, Optional<String> optionalPhoneNumber,
            Optional<String> optionalEmailId, Optional<String> optionalDateOfBirth) throws AccountException {
        if(optionalFirstName.isEmpty() && optionalLastName.isEmpty() && optionalPhoneNumber.isEmpty() && optionalEmailId.isEmpty() && optionalDateOfBirth.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String firstName = optionalFirstName.isPresent() ? optionalFirstName.get() : "";
        String lastName = optionalLastName.isPresent() ? optionalLastName.get() : "";
        String phoneNumber = optionalPhoneNumber.isPresent() ? optionalPhoneNumber.get() : "";
        String emailId = optionalEmailId.isPresent() ? optionalEmailId.get() : "";
        String dateOfBirth = optionalDateOfBirth.isPresent() ? optionalDateOfBirth.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(firstName)) && StringUtils.isEmpty(StringUtils.trimWhitespace(lastName))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(phoneNumber)) && StringUtils.isEmpty(StringUtils.trimWhitespace(emailId))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(dateOfBirth))) {
            log.debug("All search parameters are empty");
        }
        List<AccountVo> matchedAccountList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        AccountEntity entity = new AccountEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(firstName))) {
            log.debug("firstName {} is valid", firstName);
            providedFilters.put("firstName", firstName);
            entity.setFirstName(firstName);
            matcherCriteria = matcherCriteria.withMatcher("firstName", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(lastName))) {
            log.debug("lastName {} is valid", lastName);
            providedFilters.put("lastName", lastName);
            entity.setLastName(lastName);
            matcherCriteria = matcherCriteria.withMatcher("lastName", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(dateOfBirth))) {
            try {
                DateTimeFormatter dtf = DateTimeFormatter.ofPattern(dobFormat);
                LocalDate dob = LocalDate.parse(dateOfBirth, dtf);
                log.debug("dateOfBirth {} is valid", dateOfBirth);
                providedFilters.put("dateOfBirth", dateOfBirth);
                entity.setDateOfBirth(dob);
                matcherCriteria = matcherCriteria.withMatcher("dateOfBirth", match -> match.exact());
            } catch (DateTimeParseException e) {
                log.error("Unable to parse dateOfBirth", e);
                log.debug(AccountMessageTemplate.MSG_TEMPLATE_ACCOUNT_DOB_INVALID.getValue(), dateOfBirth);
                throw new AccountException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "dateOfBirth", dateOfBirth });
            }
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(phoneNumber))) {
            log.debug("phoneNumber {} is valid", phoneNumber);
            providedFilters.put("phoneNumber", phoneNumber);
            entity.setPhoneNumber(phoneNumber);
            matcherCriteria = matcherCriteria.withMatcher("phoneNumber", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(emailId))) {
            log.debug("emailId {} is valid", emailId);
            providedFilters.put("emailId", emailId);
            entity.setEmailId(emailId);
            matcherCriteria = matcherCriteria.withMatcher("emailId", match -> match.contains());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<AccountEntity> accountEntityExample = Example.of(entity, matcherCriteria);
        List<AccountEntity> accountEntityList = repository.findAll(accountEntityExample);
        matchedAccountList = customerServiceHelper.accountEntity2DetailedVo(accountEntityList);
        log.info("Found {} AccountVo matching with provided parameters : {}", matchedAccountList.size(), providedFilters);
        return matchedAccountList;
    }

    @Transactional
    @Override
    public String createAccount(AccountForm form) throws AccountException {
        log.info("Creating new AccountEntity");

        if(form == null) {
            log.debug("AccountForm provided is null");
            throw new AccountException(CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of AccountForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("AccountForm has {} errors", err.getErrorCount());
            CustomerErrorCode ec = CustomerErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("AccountForm error detail: {}", ec);
            throw new AccountException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of AccountForm are valid");

        AccountEntity expectedEntity = form2EntityConverter.convert(form);

        log.debug(AccountMessageTemplate.MSG_TEMPLATE_ACCOUNT_EXISTENCE_BY_PHONE_NUMBER.getValue(), form.getPhoneNumber());
        if(repository.existsByPhoneNumber(expectedEntity.getPhoneNumber())) {
            log.debug(AccountMessageTemplate.MSG_TEMPLATE_ACCOUNT_EXISTS_BY_PHONE_NUMBER.getValue(), expectedEntity.getPhoneNumber());
            throw new AccountException(CustomerErrorCode.CUST_EXISTS,
                    new Object[]{ "phoneNumber", form.getPhoneNumber() });
        }
        log.debug(AccountMessageTemplate.MSG_TEMPLATE_ACCOUNT_NON_EXISTENCE_BY_PHONE_NUMBER.getValue(), expectedEntity.getPhoneNumber());

        log.debug("Saving {}", expectedEntity);
        AccountEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new AccountException(CustomerErrorCode.CUST_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist AccountForm details" });
        }
        log.info("Created new AccountForm with id: {}", actualEntity.getId());
        return actualEntity.getId().toString();
    }

    @Transactional
    @Override
    public void updateAccount(String id, AccountForm form) throws AccountException {
        log.info("Updating AccountForm by id: {}", id);

        log.debug(AccountMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ACCOUNT_ENTITY_ID.getValue(), id);
        Long accountId = parseAccountId(id);
        Optional<AccountEntity> optActualEntity = repository.findById(accountId);
        if(optActualEntity.isEmpty()) {
            log.debug(AccountMessageTemplate.MSG_TEMPLATE_NO_ACCOUNT_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new AccountException(CustomerErrorCode.CUST_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(AccountMessageTemplate.MSG_TEMPLATE_FOUND_ACCOUNT_ENTITY_ID.getValue(), id);

        AccountEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("AccountEntity is inactive with id: {}", id);
            throw new AccountException(CustomerErrorCode.CUST_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("AccountEntity is active with id: {}", id);

        if(form == null) {
            log.debug("AccountForm is null");
            throw new AccountException(CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of AccountForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("AccountForm has {} errors", err.getErrorCount());
            CustomerErrorCode ec = CustomerErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("AccountForm error detail: {}", ec);
            throw new AccountException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of AccountForm are empty");
            throw new AccountException(CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of AccountForm are valid");

        Optional<AccountEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of AccountForm");
            throw new AccountException(CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from AccountForm to AccountEntity");

        AccountEntity expectedEntity = optExpectedEntity.get();

        log.debug(AccountMessageTemplate.MSG_TEMPLATE_ACCOUNT_EXISTENCE_BY_PHONE_NUMBER.getValue(), form.getPhoneNumber());
        if(actualEntity.getPhoneNumber().compareTo(expectedEntity.getPhoneNumber()) == 0
                || repository.existsByPhoneNumber(expectedEntity.getPhoneNumber())) {
            log.debug(AccountMessageTemplate.MSG_TEMPLATE_ACCOUNT_EXISTS_BY_PHONE_NUMBER.getValue(), expectedEntity.getPhoneNumber());
            throw new AccountException(CustomerErrorCode.CUST_EXISTS,
                    new Object[]{ "phoneNumber", actualEntity.getPhoneNumber() });
        }
        log.debug(AccountMessageTemplate.MSG_TEMPLATE_ACCOUNT_NON_EXISTENCE_BY_PHONE_NUMBER.getValue(), expectedEntity.getPhoneNumber());

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from AccountEntity to AccountForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new AccountException(CustomerErrorCode.CUST_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency account details" });
        }
        log.info("Updated existing AccountEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void deleteAccount(String id) throws AccountException {
        log.info("Soft deleting AccountEntity by id: {}", id);

        log.debug(AccountMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ACCOUNT_ENTITY_ID.getValue(), id);
        Long accountId = parseAccountId(id);
        Optional<AccountEntity> optEntity = repository.findById(accountId);
        if(optEntity.isEmpty()) {
            log.debug(AccountMessageTemplate.MSG_TEMPLATE_NO_ACCOUNT_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new AccountException(CustomerErrorCode.CUST_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(AccountMessageTemplate.MSG_TEMPLATE_FOUND_ACCOUNT_ENTITY_ID.getValue(), id);

        AccountEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("AccountEntity is inactive with id: {}", id);
            throw new AccountException(CustomerErrorCode.CUST_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("AccountEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        AccountEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new AccountException(CustomerErrorCode.CUST_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current account details with id:" + id });
        }

        log.info("Soft deleted existing AccountEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void applyPatchOnAccount(String id, List<PatchOperationForm> patches) throws AccountException {
        log.info("Patching AccountEntity by id: {}", id);

        log.debug(AccountMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ACCOUNT_ENTITY_ID.getValue(), id);
        Long accountId = parseAccountId(id);
        Optional<AccountEntity> optActualEntity = repository.findById(accountId);
        if(optActualEntity.isEmpty()) {
            log.debug(AccountMessageTemplate.MSG_TEMPLATE_NO_ACCOUNT_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new AccountException(CustomerErrorCode.CUST_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(AccountMessageTemplate.MSG_TEMPLATE_FOUND_ACCOUNT_ENTITY_ID.getValue(), id);

        AccountEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Account patch list not provided");
            throw new AccountException(CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Account patch list has {} items", patches.size());


        log.debug("Validating patch list items for Account");
        try {
            toabBaseService.validatePatches(patches, CustomerErrorCode.CUST_EXISTS.getDomain() + ":LOV");
            log.debug("All Account patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Account patch item are invalid");
            throw new AccountException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Account");


        log.debug("Patching list items to AccountDto");
        AccountDto patchedAccountForm = new AccountDto();
        try {
            log.debug("Preparing patch list items for Account");
            JsonNode accountDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch accountPatch = JsonPatch.fromJson(accountDtoTree);
            log.debug("Prepared patch list items for Account");
            JsonNode blankAccountDtoTree = om.convertValue(new AccountDto(), JsonNode.class);
            JsonNode patchedAccountFormTree = accountPatch.apply(blankAccountDtoTree);
            log.debug("Applying patch list items to AccountDto");
            patchedAccountForm = om.treeToValue(patchedAccountFormTree, AccountDto.class);
            log.debug("Applied patch list items to AccountDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to AccountDto: {}", e);
            AccountException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in AccountDto");
                ex = new AccountException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new AccountException(CustomerErrorCode.CUST_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to AccountDto: {}", e);
            throw new AccountException(CustomerErrorCode.CUST_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to AccountDto");

        log.debug("Validating patched AccountDto");
        Errors err = new DirectFieldBindingResult(patchedAccountForm, patchedAccountForm.getClass().getSimpleName());
        dtoValidator.validate(patchedAccountForm, err);
        if(err.hasErrors()) {
            log.debug("Patched AccountDto has {} errors", err.getErrorCount());
            CustomerErrorCode ec = CustomerErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched AccountDto error detail: {}", ec);
            throw new AccountException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched AccountDto are valid");

        log.debug(AccountMessageTemplate.MSG_TEMPLATE_ACCOUNT_EXISTENCE_BY_PHONE_NUMBER.getValue(), patchedAccountForm.getPhoneNumber().get());
        if(actualEntity.getPhoneNumber().compareTo(patchedAccountForm.getPhoneNumber().get()) == 0
                || repository.existsByPhoneNumber(patchedAccountForm.getPhoneNumber().get())) {
            log.debug(AccountMessageTemplate.MSG_TEMPLATE_ACCOUNT_EXISTS_BY_PHONE_NUMBER.getValue(), patchedAccountForm.getPhoneNumber().get());
            throw new AccountException(CustomerErrorCode.CUST_EXISTS,
                    new Object[]{ "phoneNumber", patchedAccountForm.getPhoneNumber().get() });
        }
        log.debug(AccountMessageTemplate.MSG_TEMPLATE_ACCOUNT_NON_EXISTENCE_BY_PHONE_NUMBER.getValue(), patchedAccountForm.getPhoneNumber().get());


        log.debug("Comparatively copying patched attributes from AccountDto to AccountEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedAccountForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (AccountException) e;
        }
        log.debug("Comparatively copied patched attributes from AccountDto to AccountEntity");

        log.debug("Saving patched AccountEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched AccountEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete AccountEntity with id:{}", id);
            throw new AccountException(CustomerErrorCode.CUST_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency account details with id:" + id });
        }
        log.info("Patched AccountEntity with id:{}", id);
    }
}