package com.teenthofabud.restaurant.solution.menu.category.service.impl;

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
import com.teenthofabud.restaurant.solution.menu.category.converter.CategoryDto2EntityConverter;
import com.teenthofabud.restaurant.solution.menu.category.converter.CategoryEntity2VoConverter;
import com.teenthofabud.restaurant.solution.menu.category.converter.CategoryForm2EntityConverter;
import com.teenthofabud.restaurant.solution.menu.category.data.*;
import com.teenthofabud.restaurant.solution.menu.category.mapper.CategoryEntitySelfMapper;
import com.teenthofabud.restaurant.solution.menu.category.mapper.CategoryForm2EntityMapper;
import com.teenthofabud.restaurant.solution.menu.category.repository.CategoryRepository;
import com.teenthofabud.restaurant.solution.menu.category.service.CategoryService;
import com.teenthofabud.restaurant.solution.menu.category.validator.CategoryDtoValidator;
import com.teenthofabud.restaurant.solution.menu.category.validator.CategoryFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.menu.category.validator.CategoryFormValidator;
import com.teenthofabud.restaurant.solution.menu.error.MenuErrorCode;
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
public class CategoryServiceImpl implements CategoryService {

    private static final Comparator<CategoryVo> CMP_BY_NAME = (s1, s2) -> {
        return s1.getName().compareTo(s2.getName());
    };

    private CategoryEntity2VoConverter entity2VoConverter;
    private CategoryForm2EntityConverter form2EntityConverter;
    private CategoryDto2EntityConverter dto2EntityConverter;
    private CategoryForm2EntityMapper form2EntityMapper;
    private CategoryEntitySelfMapper entitySelfMapper;
    private CategoryFormValidator formValidator;
    private CategoryFormRelaxedValidator relaxedFormValidator;
    private CategoryDtoValidator dtoValidator;
    private CategoryRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setEntity2VoConverter(CategoryEntity2VoConverter entity2VoConverter) {
        this.entity2VoConverter = entity2VoConverter;
    }

    @Autowired
    public void setDto2EntityConverter(CategoryDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(CategoryForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(CategoryEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(CategoryFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchCategoryValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(CategoryDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(CategoryForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(CategoryRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(CategoryFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private List<CategoryVo> entity2DetailedVoList(List<CategoryEntity> categoryEntityList) {
        List<CategoryVo> categoryDetailsList = new ArrayList<>(categoryEntityList.size());
        for(CategoryEntity entity : categoryEntityList) {
            CategoryVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            categoryDetailsList.add(vo);
        }
        return categoryDetailsList;
    }

    private Long parseCategoryId(String id) throws CategoryException {
        Long categoryId = null;
        try {
            categoryId = Long.parseLong(id);
            log.debug("Parsed id {} to category id {} in numeric format", id, categoryId);
            if(categoryId <= 0) {
                throw new NumberFormatException("category id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse category id", e);
            log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_ID_INVALID.getValue(), id);
            throw new CategoryException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return categoryId;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public Set<CategoryVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all CategoryEntity by their natural ordering");
        List<CategoryEntity> categoryEntityList = repository.findAll();
        Set<CategoryVo> naturallyOrderedSet = new TreeSet<CategoryVo>(CMP_BY_NAME);
        for(CategoryEntity entity : categoryEntityList) {
            CategoryVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedSet.add(dto);
        }
        log.info("{} CategoryVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public CategoryVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws CategoryException {
        log.info("Requesting CategoryEntity by id: {}", id);
        Long categoryId = parseCategoryId(id);
        Optional<CategoryEntity> optEntity = repository.findById(categoryId);
        if(optEntity.isEmpty()) {
            log.debug("No CategoryEntity found by id: {}", id);
            throw new CategoryException(MenuErrorCode.MENU_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found CategoryVo by id: {}", id);
        CategoryEntity entity = optEntity.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        CategoryVo vo = entity2VoConverter.convert(entity);
        log.debug("CategoryVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<CategoryVo> retrieveAllMatchingDetailsByCriteria(
            Optional<String> optionalName, Optional<String> optionalDescription) throws CategoryException {
        if(optionalName.isEmpty() && optionalDescription.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String description = optionalDescription.isPresent() ? optionalDescription.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(description))) {
            log.debug("All search parameters are empty");
        }
        List<CategoryVo> matchedCategoryList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        CategoryEntity entity = new CategoryEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            log.debug("name {} is valid", name);
            providedFilters.put("name", name);
            entity.setName(name);
            matcherCriteria = matcherCriteria.withMatcher("name", match -> match.contains());
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
        Example<CategoryEntity> categoryEntityExample = Example.of(entity, matcherCriteria);
        List<CategoryEntity> categoryEntityList = repository.findAll(categoryEntityExample);
        if(categoryEntityList != null && !categoryEntityList.isEmpty()) {
            matchedCategoryList = entity2DetailedVoList(categoryEntityList);
            log.info("Found {} CategoryVo matching with provided parameters : {}", matchedCategoryList.size(), providedFilters);
        }
        log.info("No CategoryVo available matching with provided parameters : {}", matchedCategoryList.size(), providedFilters);
        return matchedCategoryList;
    }

    @Transactional
    @Override
    public String createCategory(CategoryForm form) throws CategoryException {
        log.info("Creating new CategoryEntity");

        if(form == null) {
            log.debug("CategoryForm provided is null");
            throw new CategoryException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of CategoryForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("CategoryForm has {} errors", err.getErrorCount());
            MenuErrorCode ec = MenuErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("CategoryForm error detail: {}", ec);
            throw new CategoryException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of CategoryForm are valid");

        CategoryEntity expectedEntity = form2EntityConverter.convert(form);

        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_EXISTENCE_BY_NAME.getValue(), form.getName());
        if(repository.existsByName(expectedEntity.getName())) {
            log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_EXISTS_BY_NAME.getValue(), expectedEntity.getName());
            throw new CategoryException(MenuErrorCode.MENU_EXISTS,
                    new Object[]{ "name", form.getName() });
        }
        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getName());

        log.debug("Saving {}", expectedEntity);
        CategoryEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new CategoryException(MenuErrorCode.MENU_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist CategoryForm details" });
        }
        log.info("Created new CategoryForm with id: {}", actualEntity.getId());
        return actualEntity.getId().toString();
    }

    @Transactional
    @Override
    public void updateCategory(String id, CategoryForm form) throws CategoryException {
        log.info("Updating CategoryForm by id: {}", id);

        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CATEGORY_ENTITY_ID.getValue(), id);
        Long categoryId = parseCategoryId(id);
        Optional<CategoryEntity> optActualEntity = repository.findById(categoryId);
        if(optActualEntity.isEmpty()) {
            log.debug(CategoryMessageTemplate.MSG_TEMPLATE_NO_CATEGORY_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CategoryException(MenuErrorCode.MENU_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_FOUND_CATEGORY_ENTITY_ID.getValue(), id);

        CategoryEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("CategoryEntity is inactive with id: {}", id);
            throw new CategoryException(MenuErrorCode.MENU_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("CategoryEntity is active with id: {}", id);

        if(form == null) {
            log.debug("CategoryForm is null");
            throw new CategoryException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of CategoryForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("CategoryForm has {} errors", err.getErrorCount());
            MenuErrorCode ec = MenuErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("CategoryForm error detail: {}", ec);
            throw new CategoryException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of CategoryForm are empty");
            throw new CategoryException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of CategoryForm are valid");

        Optional<CategoryEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of CategoryForm");
            throw new CategoryException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from CategoryForm to CategoryEntity");

        CategoryEntity expectedEntity = optExpectedEntity.get();

        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_EXISTENCE_BY_NAME.getValue(), form.getName());
        if(actualEntity.getName().compareTo(expectedEntity.getName()) == 0
                || repository.existsByName(expectedEntity.getName())) {
            log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_EXISTS_BY_NAME.getValue(), expectedEntity.getName());
            throw new CategoryException(MenuErrorCode.MENU_EXISTS,
                    new Object[]{ "name", actualEntity.getName() });
        }
        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getName());

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from CategoryEntity to CategoryForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new CategoryException(MenuErrorCode.MENU_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency category details" });
        }
        log.info("Updated existing CategoryEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void deleteCategory(String id) throws CategoryException {
        log.info("Soft deleting CategoryEntity by id: {}", id);

        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CATEGORY_ENTITY_ID.getValue(), id);
        Long categoryId = parseCategoryId(id);
        Optional<CategoryEntity> optEntity = repository.findById(categoryId);
        if(optEntity.isEmpty()) {
            log.debug(CategoryMessageTemplate.MSG_TEMPLATE_NO_CATEGORY_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CategoryException(MenuErrorCode.MENU_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_FOUND_CATEGORY_ENTITY_ID.getValue(), id);

        CategoryEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("CategoryEntity is inactive with id: {}", id);
            throw new CategoryException(MenuErrorCode.MENU_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("CategoryEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        CategoryEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new CategoryException(MenuErrorCode.MENU_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current category details with id:" + id });
        }

        log.info("Soft deleted existing CategoryEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void applyPatchOnCategory(String id, List<PatchOperationForm> patches) throws CategoryException {
        log.info("Patching CategoryEntity by id: {}", id);

        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CATEGORY_ENTITY_ID.getValue(), id);
        Long categoryId = parseCategoryId(id);
        Optional<CategoryEntity> optActualEntity = repository.findById(categoryId);
        if(optActualEntity.isEmpty()) {
            log.debug(CategoryMessageTemplate.MSG_TEMPLATE_NO_CATEGORY_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CategoryException(MenuErrorCode.MENU_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_FOUND_CATEGORY_ENTITY_ID.getValue(), id);

        CategoryEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Category patch list not provided");
            throw new CategoryException(MenuErrorCode.MENU_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Category patch list has {} items", patches.size());


        log.debug("Validating patch list items for Category");
        try {
            toabBaseService.validatePatches(patches, MenuErrorCode.MENU_EXISTS.getDomain() + ":LOV");
            log.debug("All Category patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Category patch item are invalid");
            throw new CategoryException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Category");


        log.debug("Patching list items to CategoryDto");
        CategoryDto patchedCategoryForm = new CategoryDto();
        try {
            log.debug("Preparing patch list items for Category");
            JsonNode categoryDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch categoryPatch = JsonPatch.fromJson(categoryDtoTree);
            log.debug("Prepared patch list items for Category");
            JsonNode blankCategoryDtoTree = om.convertValue(new CategoryDto(), JsonNode.class);
            JsonNode patchedCategoryFormTree = categoryPatch.apply(blankCategoryDtoTree);
            log.debug("Applying patch list items to CategoryDto");
            patchedCategoryForm = om.treeToValue(patchedCategoryFormTree, CategoryDto.class);
            log.debug("Applied patch list items to CategoryDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to CategoryDto: {}", e);
            CategoryException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in CategoryDto");
                ex = new CategoryException(MenuErrorCode.MENU_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new CategoryException(MenuErrorCode.MENU_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to CategoryDto: {}", e);
            throw new CategoryException(MenuErrorCode.MENU_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to CategoryDto");

        log.debug("Validating patched CategoryDto");
        Errors err = new DirectFieldBindingResult(patchedCategoryForm, patchedCategoryForm.getClass().getSimpleName());
        dtoValidator.validate(patchedCategoryForm, err);
        if(err.hasErrors()) {
            log.debug("Patched CategoryDto has {} errors", err.getErrorCount());
            MenuErrorCode ec = MenuErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched CategoryDto error detail: {}", ec);
            throw new CategoryException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched CategoryDto are valid");

        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_EXISTENCE_BY_NAME.getValue(), patchedCategoryForm.getName().get());
        if(actualEntity.getName().compareTo(patchedCategoryForm.getName().get()) == 0
                || repository.existsByName(patchedCategoryForm.getName().get())) {
            log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_EXISTS_BY_NAME.getValue(), patchedCategoryForm.getName().get());
            throw new CategoryException(MenuErrorCode.MENU_EXISTS,
                    new Object[]{ "phoneNumber", patchedCategoryForm.getName().get() });
        }
        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_NON_EXISTENCE_BY_NAME.getValue(), patchedCategoryForm.getName().get());


        log.debug("Comparatively copying patched attributes from CategoryDto to CategoryEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedCategoryForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (CategoryException) e;
        }
        log.debug("Comparatively copied patched attributes from CategoryDto to CategoryEntity");

        log.debug("Saving patched CategoryEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched CategoryEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete CategoryEntity with id:{}", id);
            throw new CategoryException(MenuErrorCode.MENU_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency category details with id:" + id });
        }
        log.info("Patched CategoryEntity with id:{}", id);
    }
}