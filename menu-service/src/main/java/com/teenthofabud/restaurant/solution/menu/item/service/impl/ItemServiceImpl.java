package com.teenthofabud.restaurant.solution.menu.item.service.impl;

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
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryException;
import com.teenthofabud.restaurant.solution.menu.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.menu.category.service.CategoryService;
import com.teenthofabud.restaurant.solution.menu.error.MenuErrorCode;
import com.teenthofabud.restaurant.solution.menu.item.converter.ItemDto2EntityConverter;
import com.teenthofabud.restaurant.solution.menu.item.converter.ItemForm2EntityConverter;
import com.teenthofabud.restaurant.solution.menu.item.data.*;
import com.teenthofabud.restaurant.solution.menu.item.mapper.ItemEntitySelfMapper;
import com.teenthofabud.restaurant.solution.menu.item.mapper.ItemForm2EntityMapper;
import com.teenthofabud.restaurant.solution.menu.item.repository.ItemRepository;
import com.teenthofabud.restaurant.solution.menu.item.service.ItemService;
import com.teenthofabud.restaurant.solution.menu.item.validator.ItemDtoValidator;
import com.teenthofabud.restaurant.solution.menu.item.validator.ItemFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.menu.item.validator.ItemFormValidator;
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
public class ItemServiceImpl implements ItemService {

    private static final Comparator<ItemVo> CMP_BY_CATEGORY_ID_AND_NAME = (s1, s2) -> {
        return Integer.compare(s1.getCategoryId().compareTo(s2.getCategoryId()), s1.getName().compareTo(s2.getName()));
    };

    private ItemForm2EntityConverter form2EntityConverter;
    private ItemDto2EntityConverter dto2EntityConverter;
    private ItemForm2EntityMapper form2EntityMapper;
    private ItemEntitySelfMapper entitySelfMapper;
    private ItemFormValidator formValidator;
    private ItemFormRelaxedValidator relaxedFormValidator;
    private ItemDtoValidator dtoValidator;
    private ItemRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    private CategoryService categoryService;
    private MenuServiceHelper menuServiceHelper;

    @Autowired
    public void setMenuServiceHelper(MenuServiceHelper menuServiceHelper) {
        this.menuServiceHelper = menuServiceHelper;
    }

    @Autowired
    public void setCategoryService(CategoryService categoryService) {
        this.categoryService = categoryService;
    }

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setDto2EntityConverter(ItemDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(ItemForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(ItemEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(ItemFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchItemValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(ItemDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(ItemForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(ItemRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(ItemFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private Long parseItemId(String id) throws ItemException {
        Long itemId = null;
        try {
            itemId = Long.parseLong(id);
            log.debug("Parsed id {} to item id {} in numeric format", id, itemId);
            if(itemId <= 0) {
                throw new NumberFormatException("item id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse item id", e);
            log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_ID_INVALID.getValue(), id);
            throw new ItemException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return itemId;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public Set<ItemVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all ItemEntity by their natural ordering");
        List<ItemEntity> itemEntityList = repository.findAll();
        List<ItemVo> itemVoList = menuServiceHelper.itemEntity2DetailedVo(itemEntityList);
        Set<ItemVo> naturallyOrderedSet = new TreeSet<ItemVo>(CMP_BY_CATEGORY_ID_AND_NAME);
        naturallyOrderedSet.addAll(itemVoList);
        log.info("{} ItemVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public ItemVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws ItemException {
        log.info("Requesting ItemEntity by id: {}", id);
        Long itemId = parseItemId(id);
        Optional<ItemEntity> optEntity = repository.findById(itemId);
        if(optEntity.isEmpty()) {
            log.debug("No ItemEntity found by id: {}", id);
            throw new ItemException(MenuErrorCode.MENU_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found ItemVo by id: {}", id);
        ItemEntity entity = optEntity.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        ItemVo vo = menuServiceHelper.itemEntity2DetailedVo(entity);
        log.debug("ItemVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<ItemVo> retrieveAllMatchingDetailsByCategoryId(String categoryId, Optional<TOABCascadeLevel> optionalCascadeLevel) throws ItemException {
        log.info("Requesting ItemEntity that match with categoryId: {}", categoryId);
        Errors err = new DirectFieldBindingResult(categoryId, "ItemForm");
        try {
            CategoryVo categoryVo = categoryService.retrieveDetailsById(categoryId, Optional.of(TOABCascadeLevel.ONE));
            if(!categoryVo.getActive()) {
                throw new ItemException(MenuErrorCode.MENU_INACTIVE, new Object [] { categoryId });
            }
        } catch (CategoryException e) {
            log.error("categoryId is invalid", e);
            throw new ItemException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object [] { "categoryId: " + categoryId });
        }
        List<ItemEntity> itemEntityList = repository.findByCategoryId(Long.parseLong(categoryId));
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        List<ItemVo> matchedItemList = menuServiceHelper.itemEntity2DetailedVo(itemEntityList);
        log.info("Found {} ItemVo matching with categoryId: {}", matchedItemList.size(),categoryId);
        TOABRequestContextHolder.clearCascadeLevelContext();
        if(matchedItemList.isEmpty()) {
            log.debug("No ItemVo found matching with categoryId: {}", categoryId);
            throw new ItemException(MenuErrorCode.MENU_NOT_FOUND, new Object[] { "categoryId", categoryId });
        }
        return matchedItemList;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<ItemVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName, Optional<String> optionalDescription, Optional<String> optionalIsVegeterian) throws ItemException {
        if(optionalName.isEmpty() && optionalDescription.isEmpty() && optionalIsVegeterian.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String description = optionalDescription.isPresent() ? optionalDescription.get() : "";
        String isVegeterian = optionalIsVegeterian.isPresent() ? optionalIsVegeterian.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(description))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(isVegeterian))) {
            log.debug("All search parameters are empty");
        }
        List<ItemVo> matchedItemList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        ItemEntity entity = new ItemEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            log.debug("name {} is valid", name);
            providedFilters.put("name", name);
            entity.setName(name);
            matcherCriteria = matcherCriteria.withMatcher("name", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(isVegeterian))) {
            VegeterianStatus vegeterianStatus = null;
            try {
                vegeterianStatus = VegeterianStatus.valueOf(isVegeterian);
            } catch (IllegalArgumentException e) {
                log.error("isVegeterian parameter is invalid", e);
                throw new ItemException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "isVegeterian", isVegeterian });
            }
            Boolean vegeterianSW = VegeterianStatus.getSwitchValue(vegeterianStatus);
            log.debug("isVegeterian {} converted to {} and is valid", isVegeterian, vegeterianSW);
            providedFilters.put("isVegeterian", vegeterianSW.toString());
            entity.setIsVegeterian(vegeterianSW);
            matcherCriteria = matcherCriteria.withMatcher("isVegeterian", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(description))) {
            log.debug("description {} is valid", description);
            providedFilters.put("description", description);
            entity.setDescription(description);
            matcherCriteria = matcherCriteria.withMatcher("description", match -> match.contains());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<ItemEntity> itemEntityExample = Example.of(entity, matcherCriteria);
        List<ItemEntity> itemEntityList = repository.findAll(itemEntityExample);
        matchedItemList = menuServiceHelper.itemEntity2DetailedVo(itemEntityList);
        log.info("Found {} ItemVo matching with provided parameters : {}", matchedItemList.size(), providedFilters);
        return matchedItemList;
    }

    @Transactional
    @Override
    public String createItem(ItemForm form) throws ItemException {
        log.info("Creating new ItemEntity");

        if(form == null) {
            log.debug("ItemForm provided is null");
            throw new ItemException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of ItemForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("ItemForm has {} errors", err.getErrorCount());
            MenuErrorCode ec = MenuErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("ItemForm error detail: {}", ec);
            throw new ItemException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of ItemForm are valid");

        ItemEntity expectedEntity = form2EntityConverter.convert(form);

        log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(), form.getName(), form.getCategoryId());
        if(repository.existsByNameAndCategoryId(expectedEntity.getName(), expectedEntity.getCategory().getId())) {
            log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_EXISTS_BY_NAME_AND_CATEGORY_ID.getValue(), expectedEntity.getName(),
                    expectedEntity.getCategory().getId());
            throw new ItemException(MenuErrorCode.MENU_EXISTS,
                    new Object[]{ "name: " + form.getName(), ", categoryId: " + form.getCategoryId() });
        }
        log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_NON_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(), expectedEntity.getName(),
                expectedEntity.getCategory().getId());

        log.debug("Saving {}", expectedEntity);
        ItemEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new ItemException(MenuErrorCode.MENU_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist ItemForm details" });
        }
        log.info("Created new ItemForm with id: {}", actualEntity.getId());
        return actualEntity.getId().toString();
    }

    @Transactional
    @Override
    public void updateItem(String id, ItemForm form) throws ItemException {
        log.info("Updating ItemForm by id: {}", id);

        log.debug(ItemMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ITEM_ENTITY_ID.getValue(), id);
        Long itemId = parseItemId(id);
        Optional<ItemEntity> optActualEntity = repository.findById(itemId);
        if(optActualEntity.isEmpty()) {
            log.debug(ItemMessageTemplate.MSG_TEMPLATE_NO_ITEM_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new ItemException(MenuErrorCode.MENU_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(ItemMessageTemplate.MSG_TEMPLATE_FOUND_ITEM_ENTITY_ID.getValue(), id);

        ItemEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("ItemEntity is inactive with id: {}", id);
            throw new ItemException(MenuErrorCode.MENU_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("ItemEntity is active with id: {}", id);

        if(form == null) {
            log.debug("ItemForm is null");
            throw new ItemException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of ItemForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("ItemForm has {} errors", err.getErrorCount());
            MenuErrorCode ec = MenuErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("ItemForm error detail: {}", ec);
            throw new ItemException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of ItemForm are empty");
            throw new ItemException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of ItemForm are valid");

        Optional<ItemEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of ItemForm");
            throw new ItemException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from ItemForm to ItemEntity");

        ItemEntity expectedEntity = optExpectedEntity.get();

        checkUniquenessOfItem(form, actualEntity);

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from ItemEntity to ItemForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new ItemException(MenuErrorCode.MENU_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency item details" });
        }
        log.info("Updated existing ItemEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void deleteItem(String id) throws ItemException {
        log.info("Soft deleting ItemEntity by id: {}", id);

        log.debug(ItemMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ITEM_ENTITY_ID.getValue(), id);
        Long itemId = parseItemId(id);
        Optional<ItemEntity> optEntity = repository.findById(itemId);
        if(optEntity.isEmpty()) {
            log.debug(ItemMessageTemplate.MSG_TEMPLATE_NO_ITEM_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new ItemException(MenuErrorCode.MENU_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(ItemMessageTemplate.MSG_TEMPLATE_FOUND_ITEM_ENTITY_ID.getValue(), id);

        ItemEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("ItemEntity is inactive with id: {}", id);
            throw new ItemException(MenuErrorCode.MENU_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("ItemEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        ItemEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new ItemException(MenuErrorCode.MENU_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current item details with id:" + id });
        }

        log.info("Soft deleted existing ItemEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void applyPatchOnItem(String id, List<PatchOperationForm> patches) throws ItemException {
        log.info("Patching ItemEntity by id: {}", id);

        log.debug(ItemMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ITEM_ENTITY_ID.getValue(), id);
        Long itemId = parseItemId(id);
        Optional<ItemEntity> optActualEntity = repository.findById(itemId);
        if(optActualEntity.isEmpty()) {
            log.debug(ItemMessageTemplate.MSG_TEMPLATE_NO_ITEM_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new ItemException(MenuErrorCode.MENU_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(ItemMessageTemplate.MSG_TEMPLATE_FOUND_ITEM_ENTITY_ID.getValue(), id);

        ItemEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Item patch list not provided");
            throw new ItemException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Item patch list has {} items", patches.size());


        log.debug("Validating patch list items for Item");
        try {
            toabBaseService.validatePatches(patches, MenuErrorCode.MENU_EXISTS.getDomain() + ":LOV");
            log.debug("All Item patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Item patch item are invalid");
            throw new ItemException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Item");


        log.debug("Patching list items to ItemDto");
        ItemDto patchedItemForm = new ItemDto();
        try {
            log.debug("Preparing patch list items for Item");
            JsonNode itemDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch itemPatch = JsonPatch.fromJson(itemDtoTree);
            log.debug("Prepared patch list items for Item");
            JsonNode blankItemDtoTree = om.convertValue(new ItemDto(), JsonNode.class);
            JsonNode patchedItemFormTree = itemPatch.apply(blankItemDtoTree);
            log.debug("Applying patch list items to ItemDto");
            patchedItemForm = om.treeToValue(patchedItemFormTree, ItemDto.class);
            log.debug("Applied patch list items to ItemDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to ItemDto: {}", e);
            ItemException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in ItemDto");
                ex = new ItemException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new ItemException(MenuErrorCode.MENU_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to ItemDto: {}", e);
            throw new ItemException(MenuErrorCode.MENU_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to ItemDto");

        log.debug("Validating patched ItemDto");
        Errors err = new DirectFieldBindingResult(patchedItemForm, patchedItemForm.getClass().getSimpleName());
        dtoValidator.validate(patchedItemForm, err);
        if(err.hasErrors()) {
            log.debug("Patched ItemDto has {} errors", err.getErrorCount());
            MenuErrorCode ec = MenuErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched ItemDto error detail: {}", ec);
            throw new ItemException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched ItemDto are valid");

        checkUniquenessOfItem(patchedItemForm, actualEntity);

        log.debug("Comparatively copying patched attributes from ItemDto to ItemEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedItemForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (ItemException) e;
        }
        log.debug("Comparatively copied patched attributes from ItemDto to ItemEntity");

        log.debug("Saving patched ItemEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched ItemEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete ItemEntity with id:{}", id);
            throw new ItemException(MenuErrorCode.MENU_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency item details with id:" + id });
        }
        log.info("Patched ItemEntity with id:{}", id);
    }

    private void checkUniquenessOfItem(ItemDto patchedItemForm, ItemEntity actualEntity) throws ItemException {
        // name = true, categoryId = false
        if(patchedItemForm.getName().isPresent() && patchedItemForm.getCategoryId().isEmpty()) {
            log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    patchedItemForm.getName().get(), actualEntity.getCategory().getId());
            boolean sameEntitySw = patchedItemForm.getName().get().compareTo(actualEntity.getName()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCategoryId(patchedItemForm.getName().get(), actualEntity.getCategory().getId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_EXISTS_BY_NAME_AND_CATEGORY_ID.getValue(),
                        patchedItemForm.getName().get(), actualEntity.getCategory().getId());
                throw new ItemException(MenuErrorCode.MENU_EXISTS,
                        new Object[]{ "name: " + patchedItemForm.getName().get(), ", categoryId: " + actualEntity.getCategory().getId() });
            }
            log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_NON_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    patchedItemForm.getName().get(), actualEntity.getCategory().getId());
        }

        // name = true, categoryId = true
        if(patchedItemForm.getName().isPresent() && patchedItemForm.getCategoryId().isPresent()) {
            log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    patchedItemForm.getName().get(), patchedItemForm.getCategoryId().get());
            boolean sameEntitySw = patchedItemForm.getName().get().compareTo(actualEntity.getName().toString()) == 0
                    && patchedItemForm.getCategoryId().get().compareTo(actualEntity.getCategory().getId().toString()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCategoryId(patchedItemForm.getName().get(),
                    Long.parseLong(patchedItemForm.getCategoryId().get()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_EXISTS_BY_NAME_AND_CATEGORY_ID.getValue(),
                        patchedItemForm.getName().get(), patchedItemForm.getCategoryId().get());
                throw new ItemException(MenuErrorCode.MENU_EXISTS,
                        new Object[]{ "name: " + patchedItemForm.getName().get(), ", categoryId: " + patchedItemForm.getCategoryId().get() });
            }
            log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_NON_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    patchedItemForm.getName().get(), patchedItemForm.getCategoryId().get());
        }

        // name = false, categoryId = true
        if(patchedItemForm.getName().isEmpty() && patchedItemForm.getCategoryId().isPresent()) {
            log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    actualEntity.getName(),  patchedItemForm.getCategoryId().get());
            boolean sameEntitySw = patchedItemForm.getCategoryId().get().compareTo(actualEntity.getCategory().getId().toString()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCategoryId(
                    actualEntity.getName(), Long.parseLong(patchedItemForm.getCategoryId().get()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_EXISTS_BY_NAME_AND_CATEGORY_ID.getValue(),
                        actualEntity.getName(), patchedItemForm.getCategoryId().get());
                throw new ItemException(MenuErrorCode.MENU_EXISTS, new Object[]{ "name " + actualEntity.getName(),
                        ", categoryId: " + patchedItemForm.getCategoryId().get() });
            }
            log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_NON_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    actualEntity.getName(),  patchedItemForm.getCategoryId().get());
        }
    }

    private void checkUniquenessOfItem(ItemForm itemForm, ItemEntity actualEntity) throws ItemException {
        // name = true, categoryId = false
        if(StringUtils.hasText(StringUtils.trimWhitespace(itemForm.getName()))
                && StringUtils.isEmpty(StringUtils.trimWhitespace(itemForm.getCategoryId()))) {
            log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    itemForm.getName(), actualEntity.getCategory().getId());
            boolean sameEntitySw = itemForm.getName().compareTo(actualEntity.getName()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCategoryId(
                    itemForm.getName(), actualEntity.getCategory().getId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_EXISTS_BY_NAME_AND_CATEGORY_ID.getValue(),
                        itemForm.getName(), actualEntity.getCategory().getId());
                throw new ItemException(MenuErrorCode.MENU_EXISTS,
                        new Object[]{ "name: " + itemForm.getName(), ", categoryId: " + actualEntity.getCategory().getId() });
            }
            log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_NON_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    itemForm.getName(), actualEntity.getCategory().getId());
        }

        // name = true, categoryId = true
        if(StringUtils.hasText(StringUtils.trimWhitespace(itemForm.getName()))
                && StringUtils.hasText(StringUtils.trimWhitespace(itemForm.getCategoryId()))) {
            log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    itemForm.getName(), itemForm.getCategoryId());
            boolean sameEntitySw = itemForm.getName().compareTo(actualEntity.getName()) == 0
                    && itemForm.getCategoryId().compareTo(actualEntity.getCategory().getId().toString()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCategoryId(itemForm.getName(), Long.parseLong(itemForm.getCategoryId()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_EXISTS_BY_NAME_AND_CATEGORY_ID.getValue(),
                        itemForm.getName(), itemForm.getCategoryId());
                throw new ItemException(MenuErrorCode.MENU_EXISTS,
                        new Object[]{ "categoryId: " + itemForm.getCategoryId(), ", name: " + itemForm.getName() });
            }
            log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_NON_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    itemForm.getName(), itemForm.getCategoryId());
        }

        // name = false, categoryId = true
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(itemForm.getName()))
                && StringUtils.hasText(StringUtils.trimWhitespace(itemForm.getCategoryId()))) {
            log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    actualEntity.getName(),  itemForm.getCategoryId());
            boolean sameEntitySw = itemForm.getName().compareTo(actualEntity.getName()) == 0
                    && itemForm.getCategoryId().compareTo(actualEntity.getCategory().getId().toString()) == 0;
            boolean duplicateEntitySw =  repository.existsByNameAndCategoryId(actualEntity.getName(), Long.parseLong(itemForm.getCategoryId()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_EXISTS_BY_NAME_AND_CATEGORY_ID.getValue(),
                        actualEntity.getName(),  itemForm.getCategoryId());
                throw new ItemException(MenuErrorCode.MENU_EXISTS, new Object[]{ "name: " + actualEntity.getName(),
                        ", categoryId: " + itemForm.getCategoryId() });
            }
            log.debug(ItemMessageTemplate.MSG_TEMPLATE_ITEM_NON_EXISTENCE_BY_NAME_AND_CATEGORY_ID.getValue(),
                    actualEntity.getName(),  itemForm.getCategoryId());
        }
    }

}