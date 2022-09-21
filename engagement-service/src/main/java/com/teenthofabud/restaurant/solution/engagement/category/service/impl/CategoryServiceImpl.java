package com.teenthofabud.restaurant.solution.engagement.category.service.impl;

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
import com.teenthofabud.restaurant.solution.checkin.error.CheckInErrorCode;
import com.teenthofabud.restaurant.solution.engagement.category.converter.CategoryDto2DocumentConverter;
import com.teenthofabud.restaurant.solution.engagement.category.converter.CategoryForm2DocumentConverter;
import com.teenthofabud.restaurant.solution.checkin.category.data.*;
import com.teenthofabud.restaurant.solution.engagement.category.data.*;
import com.teenthofabud.restaurant.solution.engagement.category.mapper.CategoryDocumentSelfMapper;
import com.teenthofabud.restaurant.solution.engagement.category.mapper.CategoryForm2DocumentMapper;
import com.teenthofabud.restaurant.solution.engagement.category.repository.CategoryRepository;
import com.teenthofabud.restaurant.solution.engagement.category.service.CategoryService;
import com.teenthofabud.restaurant.solution.engagement.category.validator.CategoryDtoValidator;
import com.teenthofabud.restaurant.solution.engagement.category.validator.CategoryFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.engagement.category.validator.CategoryFormValidator;
import com.teenthofabud.restaurant.solution.checkin.utils.CheckInServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Component;
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

    private CategoryForm2DocumentConverter form2DocumentConverter;
    private CategoryDto2DocumentConverter dto2DocumentConverter;
    private CategoryForm2DocumentMapper form2DocumentMapper;
    private CategoryDocumentSelfMapper documentSelfMapper;
    private CategoryFormValidator formValidator;
    private CategoryFormRelaxedValidator relaxedFormValidator;
    private CategoryDtoValidator dtoValidator;
    private CategoryRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    private CheckInServiceHelper checkInServiceHelper;

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }


    @Autowired
    public void setSessionServiceHelper(CheckInServiceHelper checkInServiceHelper) {
        this.checkInServiceHelper = checkInServiceHelper;
    }

    @Autowired
    public void setDto2DocumentConverter(CategoryDto2DocumentConverter dto2DocumentConverter) {
        this.dto2DocumentConverter = dto2DocumentConverter;
    }

    @Autowired
    public void setForm2DocumentMapper(CategoryForm2DocumentMapper form2DocumentMapper) {
        this.form2DocumentMapper = form2DocumentMapper;
    }

    @Autowired
    public void setDocumentSelfMapper(CategoryDocumentSelfMapper documentSelfMapper) {
        this.documentSelfMapper = documentSelfMapper;
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
    public void setForm2DocumentConverter(CategoryForm2DocumentConverter form2DocumentConverter) {
        this.form2DocumentConverter = form2DocumentConverter;
    }

    @Autowired
    public void setRepository(CategoryRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(CategoryFormValidator formValidator) {
        this.formValidator = formValidator;
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
            throw new CategoryException(CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return categoryId;
    }

    @Override
    public Set<CategoryVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all CategoryDocument by their natural ordering");
        List<CategoryDocument> categoryDocumentList = repository.findAll();
        List<CategoryVo> categoryVoList = checkInServiceHelper.categoryDocument2DetailedVo(categoryDocumentList);
        Set<CategoryVo> naturallyOrderedSet = new TreeSet<>(CMP_BY_NAME);
        naturallyOrderedSet.addAll(categoryVoList);
        log.info("{} CategoryVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    public CategoryVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws CategoryException {
        log.info("Requesting CategoryDocument by id: {}", id);
        Optional<CategoryDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug("No CategoryDocument found by id: {}", id);
            throw new CategoryException(CheckInErrorCode.RESERVATION_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found CategoryVo by id: {}", id);
        CategoryDocument document = optDocument.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        CategoryVo vo = checkInServiceHelper.categoryDocument2DetailedVo(document);
        log.debug("CategoryVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

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
        CategoryDocument document = new CategoryDocument();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            log.debug("name {} is valid", name);
            providedFilters.put("name", name);
            document.setName(name);
            matcherCriteria = matcherCriteria.withMatcher("name", match -> match.contains());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(description))) {
            log.debug("description {} is valid", description);
            providedFilters.put("description", description);
            document.setDescription(description);
            matcherCriteria = matcherCriteria.withMatcher("description", match -> match.contains());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<CategoryDocument> categoryDocumentExample = Example.of(document, matcherCriteria);
        List<CategoryDocument> categoryDocumentList = repository.findAll(categoryDocumentExample);
        matchedCategoryList = checkInServiceHelper.categoryDocument2DetailedVo(categoryDocumentList);
        log.info("Found {} CategoryVo matching with provided parameters : {}", matchedCategoryList.size(), providedFilters);
        log.info("No CategoryVo available matching with provided parameters : {}", matchedCategoryList.size(), providedFilters);
        return matchedCategoryList;
    }

    @Override
    public String createCategory(CategoryForm form) throws CategoryException {
        log.info("Creating new CategoryDocument");

        if(form == null) {
            log.debug("CategoryForm provided is null");
            throw new CategoryException(CheckInErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of CategoryForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("CategoryForm has {} errors", err.getErrorCount());
            CheckInErrorCode ec = CheckInErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("CategoryForm error detail: {}", ec);
            throw new CategoryException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of CategoryForm are valid");

        CategoryDocument expectedDocument = form2DocumentConverter.convert(form);

        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_EXISTENCE_BY_NAME.getValue(), form.getName());
        if(repository.existsByName(expectedDocument.getName())) {
            log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_EXISTS_BY_NAME.getValue(), expectedDocument.getName());
            throw new CategoryException(CheckInErrorCode.RESERVATION_EXISTS,
                    new Object[]{ "name", form.getName() });
        }
        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_NON_EXISTENCE_BY_NAME.getValue(), expectedDocument.getName());

        log.debug("Saving {}", expectedDocument);
        CategoryDocument actualDocument = repository.save(expectedDocument);
        log.debug("Saved {}", actualDocument);

        if(actualDocument == null) {
            log.debug("Unable to create {}", expectedDocument);
            throw new CategoryException(CheckInErrorCode.RESERVATION_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist CategoryForm details" });
        }
        log.info("Created new CategoryForm with id: {}", actualDocument.getId());
        return actualDocument.getId().toString();
    }

    @Override
    public void updateCategory(String id, CategoryForm form) throws CategoryException {
        log.info("Updating CategoryForm by id: {}", id);

        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CATEGORY_ENTITY_ID.getValue(), id);
        Optional<CategoryDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(CategoryMessageTemplate.MSG_TEMPLATE_NO_CATEGORY_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CategoryException(CheckInErrorCode.RESERVATION_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_FOUND_CATEGORY_ENTITY_ID.getValue(), id);

        CategoryDocument actualDocument = optActualDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("CategoryDocument is inactive with id: {}", id);
            throw new CategoryException(CheckInErrorCode.RESERVATION_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("CategoryDocument is active with id: {}", id);

        if(form == null) {
            log.debug("CategoryForm is null");
            throw new CategoryException(CheckInErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of CategoryForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("CategoryForm has {} errors", err.getErrorCount());
            CheckInErrorCode ec = CheckInErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("CategoryForm error detail: {}", ec);
            throw new CategoryException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of CategoryForm are empty");
            throw new CategoryException(CheckInErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of CategoryForm are valid");

        Optional<CategoryDocument> optExpectedDocument = form2DocumentMapper.compareAndMap(actualDocument, form);
        if(optExpectedDocument.isEmpty()) {
            log.debug("No new value for attributes of CategoryForm");
            throw new CategoryException(CheckInErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from CategoryForm to CategoryDocument");

        CategoryDocument expectedDocument = optExpectedDocument.get();

        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_EXISTENCE_BY_NAME.getValue(), form.getName());
        if(actualDocument.getName().compareTo(expectedDocument.getName()) == 0
                || repository.existsByName(expectedDocument.getName())) {
            log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_EXISTS_BY_NAME.getValue(), expectedDocument.getName());
            throw new CategoryException(CheckInErrorCode.RESERVATION_EXISTS,
                    new Object[]{ "name", actualDocument.getName() });
        }
        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_NON_EXISTENCE_BY_NAME.getValue(), expectedDocument.getName());

        documentSelfMapper.compareAndMap(expectedDocument, actualDocument);
        log.debug("Compared and copied attributes from CategoryDocument to CategoryForm");
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Updated: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to update {}", actualDocument);
            throw new CategoryException(CheckInErrorCode.RESERVATION_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency category details" });
        }
        log.info("Updated existing CategoryDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void deleteCategory(String id) throws CategoryException {
        log.info("Soft deleting CategoryDocument by id: {}", id);

        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CATEGORY_ENTITY_ID.getValue(), id);
        Optional<CategoryDocument> optDocument = repository.findById(id);
        if(optDocument.isEmpty()) {
            log.debug(CategoryMessageTemplate.MSG_TEMPLATE_NO_CATEGORY_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CategoryException(CheckInErrorCode.RESERVATION_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_FOUND_CATEGORY_ENTITY_ID.getValue(), id);

        CategoryDocument actualDocument = optDocument.get();
        if(!actualDocument.getActive()) {
            log.debug("CategoryDocument is inactive with id: {}", id);
            throw new CategoryException(CheckInErrorCode.RESERVATION_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("CategoryDocument is active with id: {}", id);

        actualDocument.setActive(Boolean.FALSE);
        actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualDocument);
        CategoryDocument expectedDocument = repository.save(actualDocument);
        log.debug("Soft deleted: {}", expectedDocument);
        if(expectedDocument == null) {
            log.debug("Unable to soft delete {}", actualDocument);
            throw new CategoryException(CheckInErrorCode.RESERVATION_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current category details with id:" + id });
        }

        log.info("Soft deleted existing CategoryDocument with id: {}", actualDocument.getId());
    }

    @Override
    public void applyPatchOnCategory(String id, List<PatchOperationForm> patches) throws CategoryException {
        log.info("Patching CategoryDocument by id: {}", id);

        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CATEGORY_ENTITY_ID.getValue(), id);
        Optional<CategoryDocument> optActualDocument = repository.findById(id);
        if(optActualDocument.isEmpty()) {
            log.debug(CategoryMessageTemplate.MSG_TEMPLATE_NO_CATEGORY_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CategoryException(CheckInErrorCode.RESERVATION_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_FOUND_CATEGORY_ENTITY_ID.getValue(), id);

        CategoryDocument actualDocument = optActualDocument.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Category patch list not provided");
            throw new CategoryException(CheckInErrorCode.RESERVATION_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Category patch list has {} items", patches.size());


        log.debug("Validating patch list items for Category");
        try {
            toabBaseService.validatePatches(patches, CheckInErrorCode.RESERVATION_EXISTS.getDomain() + ":LOV");
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
                ex = new CategoryException(CheckInErrorCode.RESERVATION_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new CategoryException(CheckInErrorCode.RESERVATION_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to CategoryDto: {}", e);
            throw new CategoryException(CheckInErrorCode.RESERVATION_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to CategoryDto");

        log.debug("Validating patched CategoryDto");
        Errors err = new DirectFieldBindingResult(patchedCategoryForm, patchedCategoryForm.getClass().getSimpleName());
        dtoValidator.validate(patchedCategoryForm, err);
        if(err.hasErrors()) {
            log.debug("Patched CategoryDto has {} errors", err.getErrorCount());
            CheckInErrorCode ec = CheckInErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched CategoryDto error detail: {}", ec);
            throw new CategoryException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched CategoryDto are valid");

        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_EXISTENCE_BY_NAME.getValue(), patchedCategoryForm.getName().get());
        if(actualDocument.getName().compareTo(patchedCategoryForm.getName().get()) == 0
                || repository.existsByName(patchedCategoryForm.getName().get())) {
            log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_EXISTS_BY_NAME.getValue(), patchedCategoryForm.getName().get());
            throw new CategoryException(CheckInErrorCode.RESERVATION_EXISTS,
                    new Object[]{ "name", patchedCategoryForm.getName().get() });
        }
        log.debug(CategoryMessageTemplate.MSG_TEMPLATE_CATEGORY_NON_EXISTENCE_BY_NAME.getValue(), patchedCategoryForm.getName().get());


        log.debug("Comparatively copying patched attributes from CategoryDto to CategoryDocument");
        try {
            dto2DocumentConverter.compareAndMap(patchedCategoryForm, actualDocument);
        } catch (TOABBaseException e) {
            throw (CategoryException) e;
        }
        log.debug("Comparatively copied patched attributes from CategoryDto to CategoryDocument");

        log.debug("Saving patched CategoryDocument: {}", actualDocument);
        actualDocument = repository.save(actualDocument);
        log.debug("Saved patched CategoryDocument: {}", actualDocument);
        if(actualDocument == null) {
            log.debug("Unable to patch delete CategoryDocument with id:{}", id);
            throw new CategoryException(CheckInErrorCode.RESERVATION_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency category details with id:" + id });
        }
        log.info("Patched CategoryDocument with id:{}", id);
    }
}