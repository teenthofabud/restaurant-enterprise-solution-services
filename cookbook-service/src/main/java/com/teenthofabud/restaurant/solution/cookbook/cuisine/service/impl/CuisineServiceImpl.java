package com.teenthofabud.restaurant.solution.cookbook.cuisine.service.impl;

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
import com.teenthofabud.restaurant.solution.cookbook.cuisine.converter.CuisineDto2EntityConverter;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.converter.CuisineForm2EntityConverter;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.data.*;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.mapper.CuisineEntitySelfMapper;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.mapper.CuisineForm2EntityMapper;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.repository.CuisineRepository;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.service.CuisineService;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.validator.CuisineDtoValidator;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.validator.CuisineFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.cookbook.cuisine.validator.CuisineFormValidator;
import com.teenthofabud.restaurant.solution.cookbook.error.CookbookErrorCode;
import com.teenthofabud.restaurant.solution.cookbook.utils.CookbookServiceHelper;
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
public class CuisineServiceImpl implements CuisineService {

    private static final Comparator<CuisineVo> CMP_BY_NAME_AND_DESCRIPTION = (s1, s2) -> {
        return Integer.compare(s1.getName().compareTo(s2.getName()), s1.getDescription().compareTo(s2.getDescription()));
    };

    private CuisineForm2EntityConverter form2EntityConverter;
    private CuisineDto2EntityConverter dto2EntityConverter;
    private CuisineForm2EntityMapper form2EntityMapper;
    private CuisineEntitySelfMapper entitySelfMapper;
    private CuisineFormValidator formValidator;
    private CuisineFormRelaxedValidator relaxedFormValidator;
    private CuisineDtoValidator dtoValidator;
    private CuisineRepository repository;
    private CookbookServiceHelper cookbookServiceHelper;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;

    @Autowired
    public void setCookbookServiceHelper(CookbookServiceHelper cookbookServiceHelper) {
        this.cookbookServiceHelper = cookbookServiceHelper;
    }

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setDto2EntityConverter(CuisineDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(CuisineForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(CuisineEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(CuisineFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchCuisineValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(CuisineDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(CuisineForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(CuisineRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(CuisineFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private Long parseCuisineId(String id) throws CuisineException {
        Long cuisineId = null;
        try {
            cuisineId = Long.parseLong(id);
            log.debug("Parsed id {} to cuisine id {} in numeric format", id, cuisineId);
            if(cuisineId <= 0) {
                throw new NumberFormatException("cuisine id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse cuisine id", e);
            log.debug(CuisineMessageTemplate.MSG_TEMPLATE_CUISINE_ID_INVALID.getValue(), id);
            throw new CuisineException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return cuisineId;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public Set<CuisineVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all CuisineEntity by their natural ordering");
        List<CuisineEntity> cuisineEntityList = repository.findAll();
        List<CuisineVo> cuisineVoList = cookbookServiceHelper.cuisineEntity2DetailedVo(cuisineEntityList);
        Collections.sort(cuisineVoList, CMP_BY_NAME_AND_DESCRIPTION);
        Set<CuisineVo> naturallyOrderedSet = new LinkedHashSet<>();
        naturallyOrderedSet.addAll(cuisineVoList);
        log.info("{} CuisineVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public CuisineVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws CuisineException {
        log.info("Requesting CuisineEntity by id: {}", id);
        Long cuisineId = parseCuisineId(id);
        Optional<CuisineEntity> optEntity = repository.findById(cuisineId);
        if(optEntity.isEmpty()) {
            log.debug("No CuisineEntity found by id: {}", id);
            throw new CuisineException(CookbookErrorCode.COOK_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found CuisineVo by id: {}", id);
        CuisineEntity entity = optEntity.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        CuisineVo vo = cookbookServiceHelper.cuisineEntity2DetailedVo(entity);
        log.debug("CuisineVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<CuisineVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalName, Optional<String> optionalDescription) throws CuisineException {
        if(optionalName.isEmpty() && optionalDescription.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String name = optionalName.isPresent() ? optionalName.get() : "";
        String description = optionalDescription.isPresent() ? optionalDescription.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(name)) && StringUtils.isEmpty(StringUtils.trimWhitespace(description))) {
            log.debug("All search parameters are empty");
        }
        List<CuisineVo> matchedCuisineList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        CuisineEntity entity = new CuisineEntity();
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
        Example<CuisineEntity> cuisineEntityExample = Example.of(entity, matcherCriteria);
        List<CuisineEntity> cuisineEntityList = repository.findAll(cuisineEntityExample);
        matchedCuisineList = cookbookServiceHelper.cuisineEntity2DetailedVo(cuisineEntityList);
        log.info("Found {} CuisineVo matching with provided parameters : {}", matchedCuisineList.size(), providedFilters);
        return matchedCuisineList;
    }

    @Transactional
    @Override
    public String createCuisine(CuisineForm form) throws CuisineException {
        log.info("Creating new CuisineEntity");

        if(form == null) {
            log.debug("CuisineForm provided is null");
            throw new CuisineException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of CuisineForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("CuisineForm has {} errors", err.getErrorCount());
            CookbookErrorCode ec = CookbookErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("CuisineForm error detail: {}", ec);
            throw new CuisineException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of CuisineForm are valid");

        CuisineEntity expectedEntity = form2EntityConverter.convert(form);

        log.debug(CuisineMessageTemplate.MSG_TEMPLATE_CUISINE_EXISTENCE_BY_NAME.getValue(), form.getName());
        if(repository.existsByName(expectedEntity.getName())) {
            log.debug(CuisineMessageTemplate.MSG_TEMPLATE_CUISINE_EXISTS_BY_NAME.getValue(), expectedEntity.getName());
            throw new CuisineException(CookbookErrorCode.COOK_EXISTS,
                    new Object[]{ "name", form.getName() });
        }
        log.debug(CuisineMessageTemplate.MSG_TEMPLATE_CUISINE_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getName());

        log.debug("Saving {}", expectedEntity);
        CuisineEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new CuisineException(CookbookErrorCode.COOK_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist CuisineForm details" });
        }
        log.info("Created new CuisineForm with id: {}", actualEntity.getId());
        return actualEntity.getId().toString();
    }

    @Transactional
    @Override
    public void updateCuisine(String id, CuisineForm form) throws CuisineException {
        log.info("Updating CuisineForm by id: {}", id);

        log.debug(CuisineMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CUISINE_ENTITY_ID.getValue(), id);
        Long cuisineId = parseCuisineId(id);
        Optional<CuisineEntity> optActualEntity = repository.findById(cuisineId);
        if(optActualEntity.isEmpty()) {
            log.debug(CuisineMessageTemplate.MSG_TEMPLATE_NO_CUISINE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CuisineException(CookbookErrorCode.COOK_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CuisineMessageTemplate.MSG_TEMPLATE_FOUND_CUISINE_ENTITY_ID.getValue(), id);

        CuisineEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("CuisineEntity is inactive with id: {}", id);
            throw new CuisineException(CookbookErrorCode.COOK_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("CuisineEntity is active with id: {}", id);

        if(form == null) {
            log.debug("CuisineForm is null");
            throw new CuisineException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of CuisineForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("CuisineForm has {} errors", err.getErrorCount());
            CookbookErrorCode ec = CookbookErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("CuisineForm error detail: {}", ec);
            throw new CuisineException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of CuisineForm are empty");
            throw new CuisineException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of CuisineForm are valid");

        Optional<CuisineEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of CuisineForm");
            throw new CuisineException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from CuisineForm to CuisineEntity");

        CuisineEntity expectedEntity = optExpectedEntity.get();

        log.debug(CuisineMessageTemplate.MSG_TEMPLATE_CUISINE_EXISTENCE_BY_NAME.getValue(), form.getName());
        if(actualEntity.getName().compareTo(expectedEntity.getName()) == 0
                || repository.existsByName(expectedEntity.getName())) {
            log.debug(CuisineMessageTemplate.MSG_TEMPLATE_CUISINE_EXISTS_BY_NAME.getValue(), expectedEntity.getName());
            throw new CuisineException(CookbookErrorCode.COOK_EXISTS,
                    new Object[]{ "name", actualEntity.getName() });
        }
        log.debug(CuisineMessageTemplate.MSG_TEMPLATE_CUISINE_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getName());

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from CuisineEntity to CuisineForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new CuisineException(CookbookErrorCode.COOK_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency cuisine details" });
        }
        log.info("Updated existing CuisineEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void deleteCuisine(String id) throws CuisineException {
        log.info("Soft deleting CuisineEntity by id: {}", id);

        log.debug(CuisineMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CUISINE_ENTITY_ID.getValue(), id);
        Long cuisineId = parseCuisineId(id);
        Optional<CuisineEntity> optEntity = repository.findById(cuisineId);
        if(optEntity.isEmpty()) {
            log.debug(CuisineMessageTemplate.MSG_TEMPLATE_NO_CUISINE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CuisineException(CookbookErrorCode.COOK_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CuisineMessageTemplate.MSG_TEMPLATE_FOUND_CUISINE_ENTITY_ID.getValue(), id);

        CuisineEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("CuisineEntity is inactive with id: {}", id);
            throw new CuisineException(CookbookErrorCode.COOK_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("CuisineEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        CuisineEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new CuisineException(CookbookErrorCode.COOK_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current cuisine details with id:" + id });
        }

        log.info("Soft deleted existing CuisineEntity with id: {}", actualEntity.getId());
    }

    @Transactional
    @Override
    public void applyPatchOnCuisine(String id, List<PatchOperationForm> patches) throws CuisineException {
        log.info("Patching CuisineEntity by id: {}", id);

        log.debug(CuisineMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_CUISINE_ENTITY_ID.getValue(), id);
        Long cuisineId = parseCuisineId(id);
        Optional<CuisineEntity> optActualEntity = repository.findById(cuisineId);
        if(optActualEntity.isEmpty()) {
            log.debug(CuisineMessageTemplate.MSG_TEMPLATE_NO_CUISINE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new CuisineException(CookbookErrorCode.COOK_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(CuisineMessageTemplate.MSG_TEMPLATE_FOUND_CUISINE_ENTITY_ID.getValue(), id);

        CuisineEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Cuisine patch list not provided");
            throw new CuisineException(CookbookErrorCode.COOK_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Cuisine patch list has {} items", patches.size());


        log.debug("Validating patch list items for Cuisine");
        try {
            toabBaseService.validatePatches(patches, CookbookErrorCode.COOK_EXISTS.getDomain() + ":LOV");
            log.debug("All Cuisine patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Cuisine patch item are invalid");
            throw new CuisineException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Cuisine");


        log.debug("Patching list items to CuisineDto");
        CuisineDto patchedCuisineForm = new CuisineDto();
        try {
            log.debug("Preparing patch list items for Cuisine");
            JsonNode cuisineDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch cuisinePatch = JsonPatch.fromJson(cuisineDtoTree);
            log.debug("Prepared patch list items for Cuisine");
            JsonNode blankCuisineDtoTree = om.convertValue(new CuisineDto(), JsonNode.class);
            JsonNode patchedCuisineFormTree = cuisinePatch.apply(blankCuisineDtoTree);
            log.debug("Applying patch list items to CuisineDto");
            patchedCuisineForm = om.treeToValue(patchedCuisineFormTree, CuisineDto.class);
            log.debug("Applied patch list items to CuisineDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to CuisineDto: {}", e);
            CuisineException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in CuisineDto");
                ex = new CuisineException(CookbookErrorCode.COOK_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new CuisineException(CookbookErrorCode.COOK_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to CuisineDto: {}", e);
            throw new CuisineException(CookbookErrorCode.COOK_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to CuisineDto");

        log.debug("Validating patched CuisineDto");
        Errors err = new DirectFieldBindingResult(patchedCuisineForm, patchedCuisineForm.getClass().getSimpleName());
        dtoValidator.validate(patchedCuisineForm, err);
        if(err.hasErrors()) {
            log.debug("Patched CuisineDto has {} errors", err.getErrorCount());
            CookbookErrorCode ec = CookbookErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched CuisineDto error detail: {}", ec);
            throw new CuisineException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched CuisineDto are valid");

        log.debug(CuisineMessageTemplate.MSG_TEMPLATE_CUISINE_EXISTENCE_BY_NAME.getValue(), patchedCuisineForm.getName().get());
        if(actualEntity.getName().compareTo(patchedCuisineForm.getName().get()) == 0
                || repository.existsByName(patchedCuisineForm.getName().get())) {
            log.debug(CuisineMessageTemplate.MSG_TEMPLATE_CUISINE_EXISTS_BY_NAME.getValue(), patchedCuisineForm.getName().get());
            throw new CuisineException(CookbookErrorCode.COOK_EXISTS,
                    new Object[]{ "name", patchedCuisineForm.getName().get() });
        }
        log.debug(CuisineMessageTemplate.MSG_TEMPLATE_CUISINE_NON_EXISTENCE_BY_NAME.getValue(), patchedCuisineForm.getName().get());


        log.debug("Comparatively copying patched attributes from CuisineDto to CuisineEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedCuisineForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (CuisineException) e;
        }
        log.debug("Comparatively copied patched attributes from CuisineDto to CuisineEntity");

        log.debug("Saving patched CuisineEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched CuisineEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete CuisineEntity with id:{}", id);
            throw new CuisineException(CookbookErrorCode.COOK_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency cuisine details with id:" + id });
        }
        log.info("Patched CuisineEntity with id:{}", id);
    }
}