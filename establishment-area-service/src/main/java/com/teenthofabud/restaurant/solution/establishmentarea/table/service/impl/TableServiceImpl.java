package com.teenthofabud.restaurant.solution.establishmentarea.table.service.impl;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.fge.jsonpatch.JsonPatch;
import com.github.fge.jsonpatch.JsonPatchException;
import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.core.common.service.TOABBaseService;
import com.teenthofabud.restaurant.solution.establishmentarea.error.EstablishmentAreaErrorCode;
import com.teenthofabud.restaurant.solution.establishmentarea.table.converter.TableDto2EntityConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.table.converter.TableEntity2VoConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.table.converter.TableForm2EntityConverter;
import com.teenthofabud.restaurant.solution.establishmentarea.table.data.*;
import com.teenthofabud.restaurant.solution.establishmentarea.table.mapper.TableEntitySelfMapper;
import com.teenthofabud.restaurant.solution.establishmentarea.table.mapper.TableForm2EntityMapper;
import com.teenthofabud.restaurant.solution.establishmentarea.table.repository.TableRepository;
import com.teenthofabud.restaurant.solution.establishmentarea.table.service.TableService;
import com.teenthofabud.restaurant.solution.establishmentarea.table.validator.TableDtoValidator;
import com.teenthofabud.restaurant.solution.establishmentarea.table.validator.TableFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.establishmentarea.table.validator.TableFormValidator;
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
import java.util.stream.Collectors;

@Component
@Slf4j
public class TableServiceImpl implements TableService {

    private static final Comparator<TableVo> CMP_BY_TABLE_NAME = (s1, s2) -> {
        return s1.getTableName().compareTo(s2.getTableName());
    };

    private TableRepository tableRepository;
    
    private TableEntity2VoConverter entity2VoConverter;
    private TableForm2EntityConverter form2EntityConverter;
    private TableDto2EntityConverter dto2EntityConverter;
    
    private TableFormValidator formValidator;
    private TableFormRelaxedValidator tableFormRelaxedValidator;
    private TableDtoValidator tableDtoValidator;

    private TableForm2EntityMapper form2EntityMapper;
    private TableEntitySelfMapper entitySelfMapper;

    private TOABBaseService toabBaseService;
    private ObjectMapper om;

    @Autowired
    public void setTableDtoValidator(TableDtoValidator tableDtoValidator) {
        this.tableDtoValidator = tableDtoValidator;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setForm2EntityMapper(TableForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(TableEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setTableFormRelaxedValidator(TableFormRelaxedValidator tableFormRelaxedValidator) {
        this.tableFormRelaxedValidator = tableFormRelaxedValidator;
    }

    @Autowired
    public void setEntity2VoConverter(TableEntity2VoConverter entity2VoConverter) {
        this.entity2VoConverter = entity2VoConverter;
    }

    @Autowired
    public void setForm2EntityConverter(TableForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setDto2EntityConverter(TableDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setFormValidator(TableFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    @Autowired
    public void setTableRepository(TableRepository tableRepository) {
        this.tableRepository = tableRepository;
    }

    @Transactional
    @Override
    public String createTable(TableForm form) throws TableException {
        log.info("Creating new TableEntity");
        log.debug("Form details: {}", form);
        log.debug("Validating provided attributes of TableForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("TableForm has {} errors", err.getErrorCount());
            EstablishmentAreaErrorCode ec = EstablishmentAreaErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TableForm error detail: {}", ec);
            throw new TableException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of TableForm are valid");

        TableEntity expectedEntity = form2EntityConverter.convert(form);

        log.debug("Saving {}", expectedEntity);
        TableEntity actualEntity = tableRepository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE,
                    new Object[]{ "creation", "Unable to persist TableForm details" });
        }
        log.info("Created new TableForm with id: {}", actualEntity.getTableId());
        return actualEntity.getTableId().toString();
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public List<TableVo> retrieveListOfAllTables() {
        log.info("Retrieving all the TableEntities");
        List<TableEntity> tableEntities = tableRepository.findAll();
        List<TableVo> naturallyOrderedList = new ArrayList<>();
        for(TableEntity entity : tableEntities) {
            TableVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedList.add(dto);
        }
        return naturallyOrderedList.stream()
                .sorted(CMP_BY_TABLE_NAME)
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true, isolation = Isolation.SERIALIZABLE)
    @Override
    public TableVo retrieveDetailsById(String id, Optional<TOABCascadeLevel> optionalCascadeLevel)
            throws TableException {
        log.info("Requesting TableEntity by id: {}", id);
        Long tableId = parseTableId(id);
        Optional<TableEntity> optEntity = tableRepository.findById(tableId);
        if(optEntity.isEmpty()) {
            log.debug("No TableEntity found by id: {}", id);
            throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND,
                    new Object[] { "id", String.valueOf(id) });
        }
        TableEntity entity = optEntity.get();
        TableVo vo = entity2VoConverter.convert(entity);
        log.info("Found TableVo by id: {}", id);
        return vo;
    }

    private Long parseTableId(String id) throws TableException {
        Long tableId = null;
        try {
            tableId = Long.parseLong(id);
            log.debug("Parsed id {} to table id {} in numeric format", id, tableId);
            if(tableId <= 0) {
                throw new NumberFormatException("table id can't be zero/negative");
            }
        } catch (NumberFormatException e) {
            log.error("Unable to parse table id", e);
            log.debug(TableMessageTemplate.MSG_TEMPLATE_TABLE_ID_INVALID.getValue(), id);
            throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID,
                    new Object[] { "id", id });
        }
        return tableId;
    }

    private List<TableVo> entity2DetailedVoList(List<TableEntity> tableEntityList) {
        List<TableVo> tableDetailsList = new ArrayList<>(tableEntityList.size());
        for(TableEntity entity : tableEntityList) {
            TableVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            tableDetailsList.add(vo);
        }
        return tableDetailsList;
    }

    @Transactional
    @Override
    public List<TableVo> retrieveAllMatchingDetailsByCriteria(Optional<String> optionalTableName) throws TableException {

        String tableName = optionalTableName.get();
        log.info("Requesting TableEntity by table name: {}", tableName);

        List<TableVo> matchedAccountList = new LinkedList<>();
        Map<String, String> providedFilters = new LinkedHashMap<>();
        TableEntity entity = new TableEntity();
        ExampleMatcher matcherCriteria = ExampleMatcher.matchingAll();
        if(StringUtils.hasText(StringUtils.trimWhitespace(tableName))) {
            log.debug("tableName {} is valid", tableName);
            providedFilters.put("flrName", tableName);
            entity.setTableName(tableName);
            matcherCriteria = matcherCriteria.withMatcher("flrName", ExampleMatcher.GenericPropertyMatchers.contains());
        }
        Example<TableEntity> accountEntityExample = Example.of(entity, matcherCriteria);
        List<TableEntity> accountEntityList = tableRepository.findAll(accountEntityExample);
        if(accountEntityList != null && !accountEntityList.isEmpty()) {
            matchedAccountList = entity2DetailedVoList(accountEntityList);
            log.info("Found {} TableVo matching with provided parameters : {}", matchedAccountList.size(), providedFilters);
        } else
            log.info("Found no TableVo available matching with provided parameters : {}", providedFilters);
        return matchedAccountList;
    }

    @Transactional
    @Override
    public void updateTable(String id, TableForm form) throws TableException {
        log.info("Updating TableEntity by id: {}", id);
        Long tableId = parseTableId(id);
        Optional<TableEntity> optEntity = tableRepository.findById(tableId);
        if(optEntity.isEmpty()) {
            log.debug("No TableEntity found by id: {}", id);
            throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND,
                    new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TableMessageTemplate.MSG_TEMPLATE_TABLE_ID_VALID.getValue(), id);

        TableEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TableEntity is inactive with id: {}", id);
            throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TableEntity is active with id: {}", id);

        if(form == null) {
            log.debug("TableForm is null");
            throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED, 
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of TableForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = tableFormRelaxedValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("TableForm has {} errors", err.getErrorCount());
            EstablishmentAreaErrorCode ec = EstablishmentAreaErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TableForm error detail: {}", ec);
            throw new TableException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of TableForm are empty");
            throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, 
                    new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of TableForm are valid");

        Optional<TableEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of TableForm");
            throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED, 
                    new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from TableForm to TableEntity");

        TableEntity expectedEntity = optExpectedEntity.get();

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from TableEntity to TableForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = tableRepository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency account details" });
        }
        log.info("Updated existing TableEntity with id: {}", actualEntity.getTableId());
    }

    @Transactional
    @Override
    public void deleteTable(String id) throws TableException {
        log.info("Soft deleting TableEntity by id: {}", id);

        log.debug(TableMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TABLE_ENTITY_ID.getValue(), id);
        Long tableId = parseTableId(id);
        Optional<TableEntity> optEntity = tableRepository.findById(tableId);
        if(optEntity.isEmpty()) {
            log.debug(TableMessageTemplate.MSG_TEMPLATE_TABLE_ID_INVALID.getValue(), id);
            throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TableMessageTemplate.MSG_TEMPLATE_FOUND_TABLE_ENTITY_ID.getValue(), id);

        TableEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TableEntity is inactive with id: {}", id);
            throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TableEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        TableEntity expectedEntity = tableRepository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current account details with id:" + id });
        }

        log.info("Soft deleted existing TableEntity with id: {}", actualEntity.getTableId());
    }

    @Transactional
    @Override
    public void applyPatchOnTable(String id, List<PatchOperationForm> patches) throws TableException {
        log.info("Patching TableEntity by id: {}", id);

        log.debug(TableMessageTemplate.MSG_TEMPLATE_FOUND_TABLE_ENTITY_ID.getValue(), id);
        Long tableId = parseTableId(id);
        Optional<TableEntity> optActualEntity = tableRepository.findById(tableId);
        if(optActualEntity.isEmpty()) {
            log.debug(TableMessageTemplate.MSG_TEMPLATE_FOUND_TABLE_ENTITY_ID.getValue(), id);
            throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TableMessageTemplate.MSG_TEMPLATE_FOUND_TABLE_ENTITY_ID.getValue(), id);

        TableEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Table patch list not provided");
            throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Table patch list has {} items", patches.size());
        log.debug("Validating patch list items for Table");
        try {
            toabBaseService.validatePatches(patches, EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_EXISTS.getDomain() + ":LOV");
            log.debug("All Table patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Table patch item are invalid");
            throw new TableException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Table");
        log.debug("Patching list items to TableDto");
        TableDto patchedTableForm = new TableDto();
        try {
            log.debug("Preparing patch list items for Table");
            JsonNode tableDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch tablePatch = JsonPatch.fromJson(tableDtoTree);
            log.debug("Prepared patch list items for Table");
            log.debug("Applying patch list items to TableDto");
            JsonNode blankTableDtoTree = om.convertValue(new TableDto(), JsonNode.class);
            JsonNode patchedTableFormTree = tablePatch.apply(blankTableDtoTree);
            patchedTableForm = om.treeToValue(patchedTableFormTree, TableDto.class);
            log.debug("Applied patch list items to TableDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to TableDto: {}", e);
            TableException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in TableDto");
                ex = new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to TableDto: {}", e);
            throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to TableDto");

        log.debug("Validating patched TableDto");
        Errors err = new DirectFieldBindingResult(patchedTableForm, patchedTableForm.getClass().getSimpleName());
        tableDtoValidator.validate(patchedTableForm, err);
        if(err.hasErrors()) {
            log.debug("Patched TableDto has {} errors", err.getErrorCount());
            EstablishmentAreaErrorCode ec = EstablishmentAreaErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched TableDto error detail: {}", ec);
            throw new TableException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched TableDto are valid");

        log.debug("Comparatively copying patched attributes from TableDto to TableEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedTableForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (TableException) e;
        }
        log.debug("Comparatively copied patched attributes from TableDto to TableEntity");

        log.debug("Saving patched TableEntity: {}", actualEntity);
        actualEntity = tableRepository.save(actualEntity);
        log.debug("Saved patched TableEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete TableEntity with id:{}", id);
            throw new TableException(EstablishmentAreaErrorCode.ESTABLISHMENT_AREA_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch table details with id:" + id });
        }
        log.info("Patched TableEntity with id:{}", id);
    }

}
