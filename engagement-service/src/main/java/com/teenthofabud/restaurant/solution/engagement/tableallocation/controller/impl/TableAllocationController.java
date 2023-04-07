package com.teenthofabud.restaurant.solution.engagement.tableallocation.controller.impl;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.controller.TableAllocationAPI;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationException;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationForm;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationMessageTemplate;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationVo;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.service.TableAllocationService;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@RestController
@Slf4j
@RequestMapping("tableAllocation")
public class TableAllocationController implements ApplicationContextAware, TableAllocationAPI {

    private ApplicationContext applicationContext;

    private TableAllocationService tableAllocationService;

    @Autowired
    public void setTableAllocationService(TableAllocationService tableAllocationService) {
        this.tableAllocationService = tableAllocationService;
    }

    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    @Override
    public CreatedVo postNewTableAllocation(@io.swagger.v3.oas.annotations.parameters.RequestBody(required = false, content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE,
            schema = @Schema(implementation = TableAllocationForm.class)) }) @RequestBody(required = false) TableAllocationForm form) throws TableAllocationException {
        log.debug("Requesting to create new tableAllocation");
        if(form != null) {
            String id = tableAllocationService.createTableAllocation(form);
            log.debug("Responding with identifier of newly created new tableAllocation");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("TableAllocationForm is null");
        throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Override
    public void putExistingTableAllocation(@PathVariable String id, @io.swagger.v3.oas.annotations.parameters.RequestBody(required = false, content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE,
            schema = @Schema(implementation = TableAllocationForm.class)) }) @RequestBody(required = false) TableAllocationForm form) throws TableAllocationException {
        log.debug("Requesting to update all attributes of existing tableAllocation");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                tableAllocationService.updateTableAllocation(id, form);
                log.debug("Responding with successful updation of attributes for existing tableAllocation");
                return;
            }
            log.debug("TableAllocationForm is null");
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_TABLE_ALLOCATION_ID_EMPTY.getValue());
        throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    @Override
    public void deleteExistingTableAllocation(@PathVariable String id) throws TableAllocationException {
        log.debug("Requesting to soft delete tableAllocation");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            tableAllocationService.deleteTableAllocation(id);
            log.debug("Responding with successful deletion of existing tableAllocation");
            return;
        }
        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_TABLE_ALLOCATION_ID_EMPTY.getValue());
        throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_BOOKING_APPLICATION_JSON_PATCH)
    @Override
    public void patchExistingTableAllocation(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList) throws TableAllocationException {
        log.debug("Requesting to patch of tableAllocation attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                tableAllocationService.applyPatchOnTableAllocation(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing tableAllocation");
                return;
            }
            log.debug("tableAllocation patch document is null");
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_TABLE_ALLOCATION_ID_EMPTY.getValue());
        throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available TableAllocations and their details ordered by accountId, sequence",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(
                            schema = @Schema(implementation = TableAllocationVo.class)))})})
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    @Override
    public Set<TableAllocationVo> getAllTableAllocationNaturallyOrdered() {
        log.debug("Requesting all available tableAllocations by their natural orders");
        Set<TableAllocationVo> naturallyOrderedTableAllocations = tableAllocationService.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available tableAllocations by their natural orders");
        return naturallyOrderedTableAllocations;
    }

    @ResponseStatus(HttpStatus.OK)
    @GetMapping("primaryFilter")
    @Override
    public List<TableAllocationVo> getAllTableAllocationsByFilters(@RequestParam(required = false) String accountId, @RequestParam(required = false) String sequence, 
                                                       @RequestParam(required = false) String notes) throws TableAllocationException {
        log.debug("Requesting all available tableAllocations with given filters");
        boolean emptyAccountId = !StringUtils.hasText(StringUtils.trimWhitespace(accountId));
        boolean emptySequence = !StringUtils.hasText(StringUtils.trimWhitespace(sequence));
        boolean emptyNotes = !StringUtils.hasText(StringUtils.trimWhitespace(notes));
        if(!emptyAccountId || !emptySequence || !emptyNotes) {
            Optional<String> optAccountId = emptyAccountId ? Optional.empty() : Optional.of(accountId);
            Optional<String> optSequence = emptySequence ? Optional.empty() : Optional.of(sequence);
            Optional<String> optNotes = emptyNotes ? Optional.empty() : Optional.of(notes);
            List<TableAllocationVo> matchedByFilter = tableAllocationService.retrieveAllMatchingDetailsByCriteria(optAccountId, optSequence, optNotes);
            log.debug("Responding with all available tableAllocations with given filters");
            return matchedByFilter;
        }
        log.debug("tableAllocation filters are empty");
        throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @ResponseStatus(HttpStatus.OK)
    @GetMapping("checkInId/{checkInId}")
    @Override
    public List<TableAllocationVo> getTableAllocationByCheckInId(String checkInId) throws TableAllocationException {
        List<TableAllocationVo> matchedByCheckInId = new ArrayList<>();
        log.debug("Requesting all available tableAllocations with given checkInId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(checkInId)) && StringUtils.hasText(StringUtils.trimWhitespace(checkInId))) {
            matchedByCheckInId = tableAllocationService.retrieveAllMatchingDetailsByCheckInId(checkInId);
            log.debug("Responding with all available tableAllocations with given checkInId");
        } else if(StringUtils.isEmpty(StringUtils.trimWhitespace(checkInId))) {
            log.debug("tableAllocation checkInId is empty");
            throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "checkInId", checkInId });
        }
        return matchedByCheckInId;
    }

    @GetMapping("{id}")
    @Override
    public TableAllocationVo getTableAllocationDetailsById(@PathVariable String id, @RequestParam(required = false) String cascadeUntilLevel) throws TableAllocationException {
        TableAllocationVo tableAllocationDetails = null;
        log.debug("Requesting all details of tableAllocation by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            tableAllocationDetails = tableAllocationService.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing tableAllocation details by id");
            return tableAllocationDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                tableAllocationDetails = tableAllocationService.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing tableAllocation details by id wth fields cascaded to given level");
                return tableAllocationDetails;
            } catch (NumberFormatException e) {
                log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_TABLE_ALLOCATION_CASCADE_LEVEL_EMPTY.getValue());
                throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(TableAllocationMessageTemplate.MSG_TEMPLATE_TABLE_ALLOCATION_ID_EMPTY.getValue());
        throw new TableAllocationException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }

}
