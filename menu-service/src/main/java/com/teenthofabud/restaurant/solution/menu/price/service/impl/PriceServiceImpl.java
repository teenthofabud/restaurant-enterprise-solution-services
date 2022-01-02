package com.teenthofabud.restaurant.solution.menu.price.service.impl;

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
import com.teenthofabud.restaurant.solution.menu.item.data.ItemEntity;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemException;
import com.teenthofabud.restaurant.solution.menu.item.data.ItemVo;
import com.teenthofabud.restaurant.solution.menu.item.repository.ItemRepository;
import com.teenthofabud.restaurant.solution.menu.item.service.ItemService;
import com.teenthofabud.restaurant.solution.menu.error.MenuErrorCode;
import com.teenthofabud.restaurant.solution.menu.price.converter.PriceDto2EntityConverter;
import com.teenthofabud.restaurant.solution.menu.price.converter.PriceForm2EntityConverter;
import com.teenthofabud.restaurant.solution.menu.price.data.*;
import com.teenthofabud.restaurant.solution.menu.price.mapper.PriceEntitySelfMapper;
import com.teenthofabud.restaurant.solution.menu.price.mapper.PriceForm2EntityMapper;
import com.teenthofabud.restaurant.solution.menu.price.repository.PriceRepository;
import com.teenthofabud.restaurant.solution.menu.price.service.PriceService;
import com.teenthofabud.restaurant.solution.menu.price.validator.PriceDtoValidator;
import com.teenthofabud.restaurant.solution.menu.price.validator.PriceFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.menu.price.validator.PriceFormValidator;
import com.teenthofabud.restaurant.solution.menu.utils.MenuServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Isolation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;

@Component
@Slf4j
public class PriceServiceImpl implements PriceService {

    private static final Comparator<PriceVo> CMP_BY_CATEGORY_ID_AND_NAME = (s1, s2) -> {
        return Integer.compare(s1.getItemId().compareTo(s2.getItemId()), s1.getCurrencyId().compareTo(s2.getCurrencyId()));
    };

    private PriceForm2EntityConverter form2EntityConverter;
    private PriceDto2EntityConverter dto2EntityConverter;
    private PriceForm2EntityMapper form2EntityMapper;
    private PriceEntitySelfMapper entitySelfMapper;
    private PriceFormValidator formValidator;
    private PriceFormRelaxedValidator relaxedFormValidator;
    private PriceDtoValidator dtoValidator;
    private PriceRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    private ItemService itemService;
    private ItemRepository itemRepository;
    private MenuServiceHelper menuServiceHelper;

    @Autowired
    public void setItemRepository(ItemRepository itemRepository) {
        this.itemRepository = itemRepository;
    }

    @Autowired
    public void setMenuServiceHelper(MenuServiceHelper menuServiceHelper) {
        this.menuServiceHelper = menuServiceHelper;
    }

    @Autowired
    public void setItemService(ItemService itemService) {
        this.itemService = itemService;
    }

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setDto2EntityConverter(PriceDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(PriceForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(PriceEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(PriceFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchPriceValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(PriceDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(PriceForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(PriceRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(PriceFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private Long parsePriceId(String id) throws PriceException {
        Long priceId = null;
        try {
            priceId = Long.parseLong(id);
            log.debug("Parsed id {} to price id {} in numeric format", id, priceId);
            if(priceId <= 0) {
                throw new NumberFormatException("price id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse price id", e);
            log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_ID_INVALID.getValue(), id);
            throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return priceId;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public Set<PriceVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all PriceEntity by their natural ordering");
        List<PriceEntity> priceEntityList = repository.findAll();
        List<PriceVo> priceVoList = menuServiceHelper.priceEntity2DetailedVo(priceEntityList);
        Set<PriceVo> naturallyOrderedSet = new TreeSet<PriceVo>(CMP_BY_CATEGORY_ID_AND_NAME);
        naturallyOrderedSet.addAll(priceVoList);
        log.info("{} PriceVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public PriceVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws PriceException {
        log.info("Requesting PriceEntity by id: {}", id);
        Long priceId = parsePriceId(id);
        Optional<PriceEntity> optEntity = repository.findById(priceId);
        if(optEntity.isEmpty()) {
            log.debug("No PriceEntity found by id: {}", id);
            throw new PriceException(MenuErrorCode.MENU_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found PriceVo by id: {}", id);
        PriceEntity entity = optEntity.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        PriceVo vo = menuServiceHelper.priceEntity2DetailedVo(entity);
        log.debug("PriceVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Override
    public List<PriceVo> retrieveAllMatchingDetailsByItemId(String itemId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws PriceException {
        log.info("Requesting PriceEntity that match with itemId: {}", itemId);
        Errors err = new DirectFieldBindingResult(itemId, "PriceForm");
        try {
            ItemVo itemVo = itemService.retrieveDetailsById(itemId, Optional.of(TOABCascadeLevel.ONE));
            if(!itemVo.getActive()) {
                throw new PriceException(MenuErrorCode.MENU_INACTIVE, new Object [] { itemId });
            }
        } catch (ItemException e) {
            log.error("itemId is invalid", e);
            throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object [] { "itemId: " + itemId });
        }
        List<PriceEntity> priceEntityList = repository.findByItemId(Long.parseLong(itemId));
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        List<PriceVo> matchedPriceList = menuServiceHelper.priceEntity2DetailedVo(priceEntityList);
        log.info("Found {} PriceVo matching with itemId: {}", matchedPriceList.size(),itemId);
        TOABRequestContextHolder.clearCascadeLevelContext();
        if(matchedPriceList.isEmpty()) {
            log.debug("No PriceVo found matching with itemId: {}", itemId);
            throw new PriceException(MenuErrorCode.MENU_NOT_FOUND, new Object[] { "itemId", itemId });
        }
        return matchedPriceList;
    }

    @Override
    public List<PriceVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalItemId, Optional<String> optionalCurrencyId) throws PriceException {
        if(optionalItemId.isEmpty() && optionalCurrencyId.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String itemId = optionalItemId.isPresent() ? optionalItemId.get() : "";
        String currencyId = optionalCurrencyId.isPresent() ? optionalCurrencyId.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(itemId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(currencyId))) {
            log.debug("All search parameters are empty");
        }
        List<PriceVo> matchedPriceList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        PriceEntity entity = new PriceEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(itemId))) {
            try {
                itemService.retrieveDetailsById(itemId, Optional.empty());
            } catch (ItemException e) {
                log.error("itemId parameter is invalid", e);
                throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "itemId", itemId });
            }
            Optional<ItemEntity> optionalItemEntity = itemRepository.findById(Long.parseLong(itemId));
            log.debug("itemId {} is valid", itemId);
            providedFilters.put("itemId", itemId);
            entity.setItem(optionalItemEntity.get());
            matcherCriteria = matcherCriteria.withMatcher("itemId", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(currencyId))) {
            if(!menuServiceHelper.isCurrencyCodeValid(currencyId)) {
                log.error("currencyId parameter is invalid");
                throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "currencyId", currencyId });
            }
            log.debug("currencyId {} is valid", currencyId);
            providedFilters.put("currencyId", currencyId);
            entity.setCurrencyId(currencyId);
            matcherCriteria = matcherCriteria.withMatcher("currencyId", match -> match.contains());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<PriceEntity> priceEntityExample = Example.of(entity, matcherCriteria);
        List<PriceEntity> priceEntityList = repository.findAll(priceEntityExample);
        matchedPriceList = menuServiceHelper.priceEntity2DetailedVo(priceEntityList);
        log.info("Found {} PriceVo matching with provided parameters : {}", matchedPriceList.size(), providedFilters);
        return matchedPriceList;
    }

    @Transactional
    @Override
    public String createPrice(PriceForm form) throws PriceException {
        log.info("Creating new PriceEntity");

        if(form == null) {
            log.debug("PriceForm provided is null");
            throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of PriceForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("PriceForm has {} errors", err.getErrorCount());
            MenuErrorCode ec = MenuErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("PriceForm error detail: {}", ec);
            throw new PriceException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of PriceForm are valid");

        PriceEntity expectedEntity = form2EntityConverter.convert(form);

        /*log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_EXISTENCE_BY_ITEM_ID_AND_CURRENCY_ID.getValue(), form.getItemId(), form.getCurrencyId());
        if(repository.existsByItemIdAndCurrencyId(expectedEntity.getItem().getId(), expectedEntity.getCurrencyId())) {
            log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_EXISTS_BY_ITEM_ID_AND_CURRENCY_ID.getValue(), expectedEntity.getItem().getId(),
                    expectedEntity.getCurrencyId());
            throw new PriceException(MenuErrorCode.MENU_EXISTS,
                    new Object[]{ "itemId: " + form.getItemId(), ", currencyId: " + form.getCurrencyId() });
        }
        log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_NON_EXISTENCE_BY_ITEM_ID_AND_CURRENCY_ID.getValue(), expectedEntity.getItem().getId(),
                expectedEntity.getCurrencyId());*/

        log.debug("Saving {}", expectedEntity);
        PriceEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new PriceException(MenuErrorCode.MENU_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist PriceForm details" });
        }
        log.info("Created new PriceForm with id: {}", actualEntity.getId());
        return actualEntity.getId().toString();
    }

    @Transactional
    @Override
    public void updatePrice(String id, PriceForm form) throws PriceException {
        log.info("Updating PriceForm by id: {}", id);

        log.debug(PriceMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_PRICE_ENTITY_ID.getValue(), id);
        Long priceId = parsePriceId(id);
        Optional<PriceEntity> optActualEntity = repository.findById(priceId);
        if(optActualEntity.isEmpty()) {
            log.debug(PriceMessageTemplate.MSG_TEMPLATE_NO_PRICE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new PriceException(MenuErrorCode.MENU_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(PriceMessageTemplate.MSG_TEMPLATE_FOUND_PRICE_ENTITY_ID.getValue(), id);

        PriceEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("PriceEntity is inactive with id: {}", id);
            throw new PriceException(MenuErrorCode.MENU_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("PriceEntity is active with id: {}", id);

        if(form == null) {
            log.debug("PriceForm is null");
            throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of PriceForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("PriceForm has {} errors", err.getErrorCount());
            MenuErrorCode ec = MenuErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("PriceForm error detail: {}", ec);
            throw new PriceException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of PriceForm are empty");
            throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of PriceForm are valid");

        Optional<PriceEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of PriceForm");
            throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from PriceForm to PriceEntity");

        PriceEntity expectedEntity = optExpectedEntity.get();

        //checkUniquenessOfPrice(form, actualEntity);

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from PriceEntity to PriceForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new PriceException(MenuErrorCode.MENU_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency price details" });
        }
        log.info("Updated existing PriceEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void deletePrice(String id) throws PriceException {
        log.info("Soft deleting PriceEntity by id: {}", id);

        log.debug(PriceMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_PRICE_ENTITY_ID.getValue(), id);
        Long priceId = parsePriceId(id);
        Optional<PriceEntity> optEntity = repository.findById(priceId);
        if(optEntity.isEmpty()) {
            log.debug(PriceMessageTemplate.MSG_TEMPLATE_NO_PRICE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new PriceException(MenuErrorCode.MENU_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(PriceMessageTemplate.MSG_TEMPLATE_FOUND_PRICE_ENTITY_ID.getValue(), id);

        PriceEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("PriceEntity is inactive with id: {}", id);
            throw new PriceException(MenuErrorCode.MENU_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("PriceEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        PriceEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new PriceException(MenuErrorCode.MENU_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current price details with id:" + id });
        }

        log.info("Soft deleted existing PriceEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void applyPatchOnPrice(String id, List<PatchOperationForm> patches) throws PriceException {
        log.info("Patching PriceEntity by id: {}", id);

        log.debug(PriceMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_PRICE_ENTITY_ID.getValue(), id);
        Long priceId = parsePriceId(id);
        Optional<PriceEntity> optActualEntity = repository.findById(priceId);
        if(optActualEntity.isEmpty()) {
            log.debug(PriceMessageTemplate.MSG_TEMPLATE_NO_PRICE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new PriceException(MenuErrorCode.MENU_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(PriceMessageTemplate.MSG_TEMPLATE_FOUND_PRICE_ENTITY_ID.getValue(), id);

        PriceEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Price patch list not provided");
            throw new PriceException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Price patch list has {} prices", patches.size());


        log.debug("Validating patch list prices for Price");
        try {
            toabBaseService.validatePatches(patches, MenuErrorCode.MENU_EXISTS.getDomain() + ":LOV");
            log.debug("All Price patch list prices are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Price patch price are invalid");
            throw new PriceException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list prices for Price");


        log.debug("Patching list prices to PriceDto");
        PriceDto patchedPriceForm = new PriceDto();
        try {
            log.debug("Preparing patch list prices for Price");
            JsonNode priceDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch pricePatch = JsonPatch.fromJson(priceDtoTree);
            log.debug("Prepared patch list prices for Price");
            JsonNode blankPriceDtoTree = om.convertValue(new PriceDto(), JsonNode.class);
            JsonNode patchedPriceFormTree = pricePatch.apply(blankPriceDtoTree);
            log.debug("Applying patch list prices to PriceDto");
            patchedPriceForm = om.treeToValue(patchedPriceFormTree, PriceDto.class);
            log.debug("Applied patch list prices to PriceDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list prices to PriceDto: {}", e);
            PriceException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in PriceDto");
                ex = new PriceException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new PriceException(MenuErrorCode.MENU_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list prices to PriceDto: {}", e);
            throw new PriceException(MenuErrorCode.MENU_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list prices to PriceDto");

        log.debug("Validating patched PriceDto");
        Errors err = new DirectFieldBindingResult(patchedPriceForm, patchedPriceForm.getClass().getSimpleName());
        dtoValidator.validate(patchedPriceForm, err);
        if(err.hasErrors()) {
            log.debug("Patched PriceDto has {} errors", err.getErrorCount());
            MenuErrorCode ec = MenuErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched PriceDto error detail: {}", ec);
            throw new PriceException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched PriceDto are valid");

        //checkUniquenessOfPrice(patchedPriceForm, actualEntity);

        log.debug("Comparatively copying patched attributes from PriceDto to PriceEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedPriceForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (PriceException) e;
        }
        log.debug("Comparatively copied patched attributes from PriceDto to PriceEntity");

        log.debug("Saving patched PriceEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched PriceEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete PriceEntity with id:{}", id);
            throw new PriceException(MenuErrorCode.MENU_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency price details with id:" + id });
        }
        log.info("Patched PriceEntity with id:{}", id);
    }

    /*private void checkUniquenessOfPrice(PriceDto patchedPriceForm, PriceEntity actualEntity) throws PriceException {
        // itemId = true, currencyId = false
        if(patchedPriceForm.getItemId().isPresent() && patchedPriceForm.getCurrencyId().isEmpty()) {
            log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_EXISTENCE_BY_ITEM_ID_AND_CURRENCY_ID.getValue(),
                    patchedPriceForm.getItemId().get(), actualEntity.getCurrencyId());
            boolean sameEntitySw = patchedPriceForm.getItemId().get().compareTo(actualEntity.getItem().getId().toString()) == 0;
            boolean duplicateEntitySw =  repository.existsByItemIdAndCurrencyId(Long.parseLong(patchedPriceForm.getItemId().get()), actualEntity.getCurrencyId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_EXISTS_BY_ITEM_ID_AND_CURRENCY_ID.getValue(),
                        patchedPriceForm.getItemId().get(), actualEntity.getCurrencyId());
                throw new PriceException(MenuErrorCode.MENU_EXISTS,
                        new Object[]{ "itemId: " + patchedPriceForm.getItemId().get(), ", currencyId: " + actualEntity.getCurrencyId() });
            }
            log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_NON_EXISTENCE_BY_ITEM_ID_AND_CURRENCY_ID.getValue(),
                    patchedPriceForm.getItemId().get(), actualEntity.getCurrencyId());
        }

        // itemId = true, currencyId = true
        if(patchedPriceForm.getItemId().isPresent() && patchedPriceForm.getCurrencyId().isPresent()) {
            log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_EXISTENCE_BY_ITEM_ID_AND_CURRENCY_ID.getValue(),
                    patchedPriceForm.getItemId().get(), patchedPriceForm.getCurrencyId().get());
            boolean sameEntitySw = patchedPriceForm.getItemId().get().compareTo(actualEntity.getItem().getId().toString()) == 0
                    && patchedPriceForm.getCurrencyId().get().compareTo(actualEntity.getCurrencyId()) == 0;
            boolean duplicateEntitySw =  repository.existsByItemIdAndCurrencyId(Long.parseLong(patchedPriceForm.getItemId().get()),
                    patchedPriceForm.getCurrencyId().get());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_EXISTS_BY_ITEM_ID_AND_CURRENCY_ID.getValue(),
                        patchedPriceForm.getItemId().get(), patchedPriceForm.getCurrencyId().get());
                throw new PriceException(MenuErrorCode.MENU_EXISTS,
                        new Object[]{ "itemId: " + patchedPriceForm.getItemId().get(), ", currencyId: " + patchedPriceForm.getCurrencyId().get() });
            }
            log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_NON_EXISTENCE_BY_ITEM_ID_AND_CURRENCY_ID.getValue(),
                    patchedPriceForm.getItemId().get(), patchedPriceForm.getCurrencyId().get());
        }

        // itemId = false, currencyId = true
        if(patchedPriceForm.getItemId().isEmpty() && patchedPriceForm.getCurrencyId().isPresent()) {
            log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_EXISTENCE_BY_ITEM_ID_AND_CURRENCY_ID.getValue(),
                    actualEntity.getItem().getId().toString(),  patchedPriceForm.getCurrencyId().get());
            boolean sameEntitySw = patchedPriceForm.getCurrencyId().get().compareTo(actualEntity.getCurrencyId()) == 0;
            boolean duplicateEntitySw =  repository.existsByItemIdAndCurrencyId(
                    actualEntity.getItem().getId(), patchedPriceForm.getCurrencyId().get());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_EXISTS_BY_ITEM_ID_AND_CURRENCY_ID.getValue(),
                        actualEntity.getItem().getId().toString(), patchedPriceForm.getCurrencyId().get());
                throw new PriceException(MenuErrorCode.MENU_EXISTS, new Object[]{ "itemId " + actualEntity.getItem().getId().toString(),
                        ", currencyId: " + patchedPriceForm.getCurrencyId().get() });
            }
            log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_NON_EXISTENCE_BY_ITEM_ID_AND_CURRENCY_ID.getValue(),
                    actualEntity.getItem().getId().toString(),  patchedPriceForm.getCurrencyId().get());
        }
    }

    private void checkUniquenessOfPrice(PriceForm priceForm, PriceEntity actualEntity) throws PriceException {
        // itemId = true, currencyId = false
        if(StringUtils.hasText(StringUtils.trimWhitespace(priceForm.getItemId()))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(priceForm.getCurrencyId()))) {
            log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_EXISTENCE_BY_ITEM_ID_AND_CURRENCY_ID.getValue(),
                    priceForm.getItemId(), actualEntity.getCurrencyId());
            boolean sameEntitySw = priceForm.getItemId().compareTo(actualEntity.getItem().getId().toString()) == 0;
            boolean duplicateEntitySw =  repository.existsByItemIdAndCurrencyId(
                    Long.parseLong(priceForm.getItemId()), actualEntity.getCurrencyId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_EXISTS_BY_ITEM_ID_AND_CURRENCY_ID.getValue(),
                        priceForm.getItemId(), actualEntity.getCurrencyId());
                throw new PriceException(MenuErrorCode.MENU_EXISTS,
                        new Object[]{ "itemId: " + priceForm.getItemId(), ", currencyId: " + actualEntity.getCurrencyId() });
            }
            log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_NON_EXISTENCE_BY_ITEM_ID_AND_CURRENCY_ID.getValue(),
                    priceForm.getItemId(), actualEntity.getCurrencyId());
        }

        // itemId = true, currencyId = true
        if(StringUtils.hasText(StringUtils.trimWhitespace(priceForm.getItemId()))
                && StringUtils.hasText(StringUtils.trimWhitespace(priceForm.getCurrencyId()))) {
            log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_EXISTENCE_BY_ITEM_ID_AND_CURRENCY_ID.getValue(),
                    priceForm.getItemId(), priceForm.getCurrencyId());
            boolean sameEntitySw = priceForm.getItemId().compareTo(actualEntity.getItem().getId().toString()) == 0
                    && priceForm.getCurrencyId().compareTo(actualEntity.getCurrencyId()) == 0;
            boolean duplicateEntitySw =  repository.existsByItemIdAndCurrencyId(Long.parseLong(priceForm.getItemId()),
                    priceForm.getCurrencyId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_EXISTS_BY_ITEM_ID_AND_CURRENCY_ID.getValue(),
                        priceForm.getItemId(), priceForm.getCurrencyId());
                throw new PriceException(MenuErrorCode.MENU_EXISTS,
                        new Object[]{ "itemId: " + priceForm.getItemId(), ", currencyId: " + priceForm.getCurrencyId() });
            }
            log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_NON_EXISTENCE_BY_ITEM_ID_AND_CURRENCY_ID.getValue(),
                    priceForm.getItemId(), priceForm.getCurrencyId());
        }

        // itemId = false, currencyId = true
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(priceForm.getItemId()))
                && StringUtils.hasText(StringUtils.trimWhitespace(priceForm.getCurrencyId()))) {
            log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_EXISTENCE_BY_ITEM_ID_AND_CURRENCY_ID.getValue(),
                    actualEntity.getItem().getId().toString(),  priceForm.getCurrencyId());
            boolean sameEntitySw = priceForm.getItemId().compareTo(actualEntity.getItem().getId().toString()) == 0
                    && priceForm.getCurrencyId().compareTo(actualEntity.getCurrencyId()) == 0;
            boolean duplicateEntitySw =  repository.existsByItemIdAndCurrencyId(actualEntity.getItem().getId(),
                    priceForm.getCurrencyId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_EXISTS_BY_ITEM_ID_AND_CURRENCY_ID.getValue(),
                        actualEntity.getItem().getId().toString(),  priceForm.getCurrencyId());
                throw new PriceException(MenuErrorCode.MENU_EXISTS, new Object[]{ "itemId: " + actualEntity.getItem().getId().toString(),
                        ", currencyId: " + priceForm.getCurrencyId() });
            }
            log.debug(PriceMessageTemplate.MSG_TEMPLATE_PRICE_NON_EXISTENCE_BY_ITEM_ID_AND_CURRENCY_ID.getValue(),
                    actualEntity.getItem().getId().toString(),  priceForm.getCurrencyId());
        }
    }*/

}