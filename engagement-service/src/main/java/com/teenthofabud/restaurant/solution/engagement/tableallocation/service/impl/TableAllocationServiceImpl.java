package com.teenthofabud.restaurant.solution.engagement.tableallocation.service.impl;

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
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.converter.TableAllocationDto2EntityConverter;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.converter.TableAllocationEntity2VoConverter;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.converter.TableAllocationForm2EntityConverter;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.*;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.mapper.TableAllocationEntitySelfMapper;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.mapper.TableAllocationForm2EntityMapper;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.repository.TableAllocationRepository;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.service.TableAllocationService;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.validator.TableAllocationDtoValidator;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.validator.TableAllocationFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.validator.TableAllocationFormValidator;
import com.teenthofabud.restaurant.solution.engagement.utils.EngagementServiceHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;

@Component
@Slf4j
public class TableAllocationServiceImpl implements InitializingBean, TableAllocationService {

    private static final Comparator<TableAllocationVo> CMP_BY_TABLE_ID_AND_ACTIVE_AND_CREATED_ON = (s1, s2) -> {
        return Integer.compare(s1.getTableId().compareTo(s2.getTableId()), s1.getCreatedOn().compareTo(s2.getCreatedOn()));
    };


    private ObjectMapper om;

    private TOABBaseService toabBaseService;

    private EngagementServiceHelper engagementServiceHelper;


    public TableAllocationFormValidator tableAllocationFormValidator;

    public TableAllocationFormRelaxedValidator tableAllocationFormRelaxedValidator;

    public TableAllocationDtoValidator tableAllocationDtoValidator;

    public TableAllocationRepository tableAllocationRepository;

    public TableAllocationEntitySelfMapper tableAllocationEntitySelfMapper;

    public TableAllocationForm2EntityMapper tableAllocationForm2EntityMapper;

    public TableAllocationForm2EntityConverter tableAllocationForm2EntityConverter;

    public TableAllocationEntity2VoConverter tableAllocationEntity2VoConverter;

    public TableAllocationDto2EntityConverter tableAllocationDto2EntityConverter;

    @Autowired
    public void setTableAllocationFormValidator(TableAllocationFormValidator tableAllocationFormValidator) {
        this.tableAllocationFormValidator = tableAllocationFormValidator;
    }

    @Autowired
    public void setTableAllocationFormRelaxedValidator(TableAllocationFormRelaxedValidator tableAllocationFormRelaxedValidator) {
        this.tableAllocationFormRelaxedValidator = tableAllocationFormRelaxedValidator;
    }

    @Autowired
    public void setTableAllocationDtoValidator(TableAllocationDtoValidator tableAllocationDtoValidator) {
        this.tableAllocationDtoValidator = tableAllocationDtoValidator;
    }

    @Autowired
    public void setTableAllocationRepository(TableAllocationRepository tableAllocationRepository) {
        this.tableAllocationRepository = tableAllocationRepository;
    }

    @Autowired
    public void setTableAllocationEntitySelfMapper(TableAllocationEntitySelfMapper tableAllocationEntitySelfMapper) {
        this.tableAllocationEntitySelfMapper = tableAllocationEntitySelfMapper;
    }

    @Autowired
    public void setTableAllocationForm2EntityMapper(TableAllocationForm2EntityMapper tableAllocationForm2EntityMapper) {
        this.tableAllocationForm2EntityMapper = tableAllocationForm2EntityMapper;
    }

    @Autowired
    public void setTableAllocationForm2EntityConverter(TableAllocationForm2EntityConverter tableAllocationForm2EntityConverter) {
        this.tableAllocationForm2EntityConverter = tableAllocationForm2EntityConverter;
    }

    @Autowired
    public void setTableAllocationEntity2VoConverter(TableAllocationEntity2VoConverter tableAllocationEntity2VoConverter) {
        this.tableAllocationEntity2VoConverter = tableAllocationEntity2VoConverter;
    }

    @Autowired
    public void setTableAllocationDto2EntityConverter(TableAllocationDto2EntityConverter tableAllocationDto2EntityConverter) {
        this.tableAllocationDto2EntityConverter = tableAllocationDto2EntityConverter;
    }

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setEngagementServiceHelper(EngagementServiceHelper engagementServiceHelper) {
        this.engagementServiceHelper = engagementServiceHelper;
    }


    private Long parsePK(String id) throws TableAllocationException {
        Long tableAllocationId = -1L;
        try {
            tableAllocationId = Long.parseLong(id);
            //log.debug("Parsed id {} to tableAllocation id {} in numeric format", id, tableAllocationId);
            if(tableAllocationId <= 0) {
                throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        } catch (NumberFormatException e) {
            //log.error("Unable to parse tableAllocation id", e);
            //log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_CHECKIN_ID_INVALID.getValue(), id);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
        }
        return tableAllocationId;
    }

    @Override
    public Set<TableAllocationVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all TableAllocationEntity by their natural ordering");
        List<TableAllocationEntity> tableAllocationEntityList = tableAllocationRepository.findAll();
        List<TableAllocationVo> tableAllocationVoList = engagementServiceHelper.tableAllocationEntity2DetailedVo(tableAllocationEntityList);
        Set<TableAllocationVo> naturallyOrderedSet = new TreeSet<>(CMP_BY_TABLE_ID_AND_ACTIVE_AND_CREATED_ON);
        naturallyOrderedSet.addAll(tableAllocationVoList);
        log.info("{} TableAllocationVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    public TableAllocationVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel) throws TableAllocationException {
        log.info("Requesting TableAllocationEntity by id: {}", id);
        Long idL = this.parsePK(id);
        Optional<TableAllocationEntity> optEntity = tableAllocationRepository.findById(idL);
        if(optEntity.isEmpty()) {
            log.debug("No TableAllocationEntity found by id: {}", id);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.info("Found TableAllocationVo by id: {}", id);
        TableAllocationEntity entity = optEntity.get();
        TOABCascadeLevel cascadeLevel = optionalCascadeLevel.isPresent() ? optionalCascadeLevel.get() : TOABCascadeLevel.ZERO;
        TOABRequestContextHolder.setCascadeLevelContext(cascadeLevel);
        TableAllocationVo vo = engagementServiceHelper.tableAllocationEntity2DetailedVo(entity);
        log.debug("TableAllocationVo populated with fields cascaded to level: {}", cascadeLevel);
        TOABRequestContextHolder.clearCascadeLevelContext();
        return vo;
    }

    @Override
    public List<TableAllocationVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalTableId,
                                                                        Optional<String> optionalActive,
                                                                        Optional<String> optionalNotes) throws TableAllocationException {
        if(optionalTableId.isEmpty() && optionalActive.isEmpty() && optionalNotes.isEmpty()) {
            log.debug("No search parameters provided");
        }
        String tableId = optionalTableId.isPresent() ? optionalTableId.get() : "";
        String active = optionalActive.isPresent() ? optionalActive.get() : "";
        String notes = optionalNotes.isPresent() ? optionalNotes.get() : "";
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(tableId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(active)) && StringUtils.isEmpty(StringUtils.trimWhitespace(notes))) {
            log.debug("All search parameters are empty");
        }
        List<TableAllocationVo> matchedTableAllocationList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        TableAllocationEntity entity = new TableAllocationEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(tableId))) {
            log.debug("tableId {} is valid", tableId);
            providedFilters.put("tableId", tableId);
            entity.setTableId(tableId);
            matcherCriteria = matcherCriteria.withMatcher("tableId", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(active))) {
            // No boolean check because see documentation of Boolean.valueOf(String)
            log.debug("active {} is valid", active);
            providedFilters.put("active", active);
            entity.setActive(Boolean.valueOf(active));
            matcherCriteria = matcherCriteria.withMatcher("active", match -> match.exact());
        }
        if(StringUtils.hasText(StringUtils.trimWhitespace(notes))) {
            log.debug("notes {} is valid", notes);
            providedFilters.put("notes", notes);
            entity.setNotes(notes);
            matcherCriteria = matcherCriteria.withMatcher("notes", match -> match.contains());
        }
        if(providedFilters.isEmpty()) {
            log.debug("search parameters are not valid");
        } else {
            log.debug("search parameters {} are valid", providedFilters);
        }
        Example<TableAllocationEntity> tableAllocationEntityExample = Example.of(entity, matcherCriteria);
        List<TableAllocationEntity> tableAllocationEntityList = tableAllocationRepository.findAll(tableAllocationEntityExample);
        matchedTableAllocationList = engagementServiceHelper.tableAllocationEntity2DetailedVo(tableAllocationEntityList);
        log.info("Found {} TableAllocationVo matching with provided parameters : {}", matchedTableAllocationList.size(), providedFilters);
        log.info("No TableAllocationVo available matching with provided parameters : {}", matchedTableAllocationList.size(), providedFilters);
        return matchedTableAllocationList;
    }

    @Override
    public List<TableAllocationVo> retrieveAllMatchingDetailsByCheckInId(String checkInId) throws TableAllocationException {
        List<TableAllocationVo> tableAllocationVos = new ArrayList<>();
        log.debug("Validating checkInId: {}", checkInId);
        if(StringUtils.isEmpty(StringUtils.trimWhitespace(checkInId))) {
            log.debug("checkInId: {} is empty", checkInId);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "checkInId", checkInId });
        }
        log.debug("checkInId: {} is valid", checkInId);

        log.info("Requesting all TableAllocationEntity by checkInId {}", checkInId);
        List<TableAllocationEntity> tableAllocationEntityList = tableAllocationRepository.findByCheckInId(checkInId);
        if(tableAllocationEntityList.isEmpty()) {
            log.debug("No TableAllocationEntity found by checkInId {}", checkInId);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "checkInId", checkInId });
        }
        log.info("Found TableAllocationVo by checkInId {}", checkInId);
        List<TableAllocationVo> tableAllocationVoList = engagementServiceHelper.tableAllocationEntity2DetailedVo(tableAllocationEntityList);
        Collections.sort(tableAllocationVoList, CMP_BY_TABLE_ID_AND_ACTIVE_AND_CREATED_ON);
        log.info("{} TableAllocationVo available", tableAllocationVoList.size());
        return tableAllocationVoList;
    }

    @Override
    public String createTableAllocation(TableAllocationForm form) throws TableAllocationException {
        log.info("Creating new TableAllocationEntity");

        if(form == null) {
            log.debug("TableAllocationForm provided is null");
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of TableAllocationForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        tableAllocationFormValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("TableAllocationForm has {} errors", err.getErrorCount());
            EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TableAllocationForm error detail: {}", ec);
            throw new TableAllocationException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of TableAllocationForm are valid");

        TableAllocationEntity expectedEntity = tableAllocationForm2EntityConverter.convert(form);
        boolean active = true;

        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_TABLE_ALLOCATION_EXISTENCE_BY_CHECK_IN_ID_AND_TABLE_ID_AND_ACTIVE.getValue(), form.getCheckInId(), form.getTableId(), active);
        if(tableAllocationRepository.existsByCheckInIdAndTableIdAndActive(expectedEntity.getCheckIn().getId(), expectedEntity.getTableId(), active)) {
            log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_TABLE_ALLOCATION_EXISTS_BY_CHECK_IN_ID_AND_TABLE_ID_AND_ACTIVE.getValue(),
                    expectedEntity.getTableId(), expectedEntity.getTableId(), active);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_EXISTS,
                    new Object[]{"tableId: " + form.getTableId(), ", sequence: " + form.getTableId() + ", active: " + active }  );
        }
        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_TABLE_ALLOCATION_NON_EXISTENCE_BY_CHECK_IN_ID_AND_TABLE_ID_AND_ACTIVE.getValue(), expectedEntity.getCheckIn().getId().toString(), expectedEntity.getTableId(), active);

        log.debug("Saving {}", expectedEntity);
        TableAllocationEntity actualEntity = (TableAllocationEntity) tableAllocationRepository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist TableAllocationForm details" });
        }
        log.info("Created new TableAllocationForm with id: {}", actualEntity.getId());
        return actualEntity.getId().toString();
    }

    @Override
    public void updateTableAllocation(String id, TableAllocationForm form) throws TableAllocationException {
        log.info("Updating TableAllocationForm by id: {}", id);

        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TABLE_ALLOCATION_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<TableAllocationEntity> optActualEntity = tableAllocationRepository.findById(idL);
        if(optActualEntity.isEmpty()) {
            log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_NO_TABLE_ALLOCATION_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_FOUND_TABLE_ALLOCATION_ENTITY_ID.getValue(), id);

        TableAllocationEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TableAllocationEntity is inactive with id: {}", id);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TableAllocationEntity is active with id: {}", id);

        if(form == null) {
            log.debug("TableAllocationForm is null");
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of TableAllocationForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = tableAllocationFormRelaxedValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("TableAllocationForm has {} errors", err.getErrorCount());
            EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TableAllocationForm error detail: {}", ec);
            throw new TableAllocationException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of TableAllocationForm are empty");
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of TableAllocationForm are valid");

        Optional<TableAllocationEntity> optExpectedEntity = tableAllocationForm2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of TableAllocationForm");
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from TableAllocationForm to TableAllocationEntity");

        TableAllocationEntity expectedEntity = optExpectedEntity.get();

        this.checkUniquenessOfTableAllocation(form, actualEntity);

        tableAllocationEntitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from TableAllocationEntity to TableAllocationForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = (TableAllocationEntity) tableAllocationRepository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency tableAllocation details" });
        }
        log.info("Updated existing TableAllocationEntity with id: {}", actualEntity.getId());
    }

    @Override
    public void deleteTableAllocation(String id) throws TableAllocationException {
        log.info("Soft deleting TableAllocationEntity by id: {}", id);

        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TABLE_ALLOCATION_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<TableAllocationEntity> optEntity = tableAllocationRepository.findById(idL);
        if(optEntity.isEmpty()) {
            log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_NO_TABLE_ALLOCATION_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_FOUND_TABLE_ALLOCATION_ENTITY_ID.getValue(), id);

        TableAllocationEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TableAllocationEntity is inactive with id: {}", id);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TableAllocationEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        TableAllocationEntity expectedEntity = (TableAllocationEntity) tableAllocationRepository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current tableAllocation details with id:" + id });
        }

        log.info("Soft deleted existing TableAllocationEntity with id: {}", actualEntity.getId());

    }

    @Override
    public void applyPatchOnTableAllocation(String id, List<PatchOperationForm> patches) throws TableAllocationException {
        log.info("Patching TableAllocationEntity by id: {}", id);

        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TABLE_ALLOCATION_ENTITY_ID.getValue(), id);
        Long idL = this.parsePK(id);
        Optional<TableAllocationEntity> optActualEntity = tableAllocationRepository.findById(idL);
        if(optActualEntity.isEmpty()) {
            log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_NO_TABLE_ALLOCATION_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_FOUND_TABLE_ALLOCATION_ENTITY_ID.getValue(), id);

        TableAllocationEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("TableAllocation patch list not provided");
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("TableAllocation patch list has {} items", patches.size());


        log.debug("Validating patch list items for TableAllocation");
        try {
            toabBaseService.validatePatches(patches, EngagementErrorCode.ENGAGEMENT_EXISTS.getDomain() + ":LOV");
            log.debug("All TableAllocation patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the TableAllocation patch item are invalid");
            throw new TableAllocationException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for TableAllocation");


        log.debug("Patching list items to TableAllocationDto");
        TableAllocationDto patchedTableAllocationForm = new TableAllocationDto();
        try {
            log.debug("Preparing patch list items for TableAllocation");
            JsonNode tableAllocationDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch tableAllocationPatch = JsonPatch.fromJson(tableAllocationDtoTree);
            log.debug("Prepared patch list items for TableAllocation");
            JsonNode blankTableAllocationDtoTree = om.convertValue(new TableAllocationDto(), JsonNode.class);
            JsonNode patchedTableAllocationFormTree = tableAllocationPatch.apply(blankTableAllocationDtoTree);
            log.debug("Applying patch list items to TableAllocationDto");
            patchedTableAllocationForm = om.treeToValue(patchedTableAllocationFormTree, TableAllocationDto.class);
            log.debug("Applied patch list items to TableAllocationDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to TableAllocationDto: {}", e);
            TableAllocationException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in TableAllocationDto");
                ex = new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to TableAllocationDto: {}", e);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to TableAllocationDto");

        log.debug("Validating patched TableAllocationDto");
        Errors err = new DirectFieldBindingResult(patchedTableAllocationForm, patchedTableAllocationForm.getClass().getSimpleName());
        tableAllocationDtoValidator.validate(patchedTableAllocationForm, err);
        if(err.hasErrors()) {
            log.debug("Patched TableAllocationDto has {} errors", err.getErrorCount());
            EngagementErrorCode ec = EngagementErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched TableAllocationDto error detail: {}", ec);
            throw new TableAllocationException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched TableAllocationDto are valid");

        this.checkUniquenessOfTableAllocation(patchedTableAllocationForm, actualEntity);

        log.debug("Comparatively copying patched attributes from TableAllocationDto to TableAllocationEntity");
        try {
            tableAllocationDto2EntityConverter.compareAndMap(patchedTableAllocationForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (TableAllocationException) e;
        }
        log.debug("Comparatively copied patched attributes from TableAllocationDto to TableAllocationEntity");

        log.debug("Saving patched TableAllocationEntity: {}", actualEntity);
        actualEntity = (TableAllocationEntity) tableAllocationRepository.save(actualEntity);
        log.debug("Saved patched TableAllocationEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete TableAllocationEntity with id:{}", id);
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency tableAllocation details with id:" + id });
        }
        log.info("Patched TableAllocationEntity with id:{}", id);
    }

    private void checkUniquenessOfTableAllocation(TableAllocationDto patchedTableAllocationForm, TableAllocationEntity actualEntity) throws TableAllocationException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(2);
        if(patchedTableAllocationForm.getTableId().isPresent()) {
            similaritySwitchesCollection.add(patchedTableAllocationForm.getTableId().get().compareTo(actualEntity.getTableId()) == 0);
        }
        if(patchedTableAllocationForm.getCheckInId().isPresent()) {
            similaritySwitchesCollection.add(patchedTableAllocationForm.getCheckInId().get().compareTo(actualEntity.getCheckIn().getId().toString()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String tableId = patchedTableAllocationForm.getTableId().isPresent() ? patchedTableAllocationForm.getTableId().get() : actualEntity.getTableId();
            Long checkInId = patchedTableAllocationForm.getCheckInId().isPresent() ? Long.parseLong(patchedTableAllocationForm.getCheckInId().get()) : actualEntity.getCheckIn().getId();
            boolean active = true;
            log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_TABLE_ALLOCATION_EXISTENCE_BY_CHECK_IN_ID_AND_TABLE_ID_AND_ACTIVE.getValue(), checkInId, tableId, active);
            boolean sameEntitySw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateEntitySw =  tableAllocationRepository.existsByCheckInIdAndTableIdAndActive(checkInId, tableId, active);
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_TABLE_ALLOCATION_EXISTS_BY_CHECK_IN_ID_AND_TABLE_ID_AND_ACTIVE.getValue(), checkInId, tableId, active);
                throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_EXISTS, new Object[]{ "checkInId: " + checkInId, ", tableId: " + tableId + ", active" + active });
            }
            log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_TABLE_ALLOCATION_NON_EXISTENCE_BY_CHECK_IN_ID_AND_TABLE_ID_AND_ACTIVE.getValue(), checkInId, tableId, active);

        }
    }

    private void checkUniquenessOfTableAllocation(TableAllocationForm tableAllocationForm, TableAllocationEntity actualEntity) throws TableAllocationException {
        List<Boolean> similaritySwitchesCollection = new ArrayList<>(3);
        if(StringUtils.hasText(StringUtils.trimWhitespace(tableAllocationForm.getTableId()))) {
            similaritySwitchesCollection.add(tableAllocationForm.getTableId().compareTo(actualEntity.getTableId()) == 0);
        }
        if(!ObjectUtils.isEmpty(tableAllocationForm.getCheckInId())) {
            similaritySwitchesCollection.add(tableAllocationForm.getCheckInId().compareTo(actualEntity.getCheckIn().getId().toString()) == 0);
        }
        if(!similaritySwitchesCollection.isEmpty()) {
            String tableId = StringUtils.hasText(StringUtils.trimWhitespace(tableAllocationForm.getTableId())) ? tableAllocationForm.getTableId() : actualEntity.getTableId();
            Long checkInId = !ObjectUtils.isEmpty(tableAllocationForm.getCheckInId()) ? Long.parseLong(tableAllocationForm.getCheckInId()) : actualEntity.getCheckIn().getId();
            boolean active = true;
            log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_TABLE_ALLOCATION_EXISTENCE_BY_CHECK_IN_ID_AND_TABLE_ID_AND_ACTIVE.getValue(), checkInId, tableId, active);
            boolean sameEntitySw = similaritySwitchesCollection.stream().allMatch(Boolean::valueOf);
            boolean duplicateEntitySw =  tableAllocationRepository.existsByCheckInIdAndTableIdAndActive(checkInId, tableId, active);
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_TABLE_ALLOCATION_EXISTS_BY_CHECK_IN_ID_AND_TABLE_ID_AND_ACTIVE.getValue(), checkInId, tableId, active);
                throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_EXISTS, new Object[]{ "checkInId: " + checkInId, ", tableId: " + tableId + ", active: " + active});
            }
            log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_TABLE_ALLOCATION_NON_EXISTENCE_BY_CHECK_IN_ID_AND_TABLE_ID_AND_ACTIVE.getValue(), checkInId, tableId, active);

        }
    }

    @Override
    public void afterPropertiesSet() throws Exception {
        this.om = new ObjectMapper();
    }
}