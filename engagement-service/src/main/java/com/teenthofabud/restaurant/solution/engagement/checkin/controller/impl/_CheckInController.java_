package com.teenthofabud.restaurant.solution.engagement.checkin.controller.impl;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.controller.CheckInAPI;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.CheckInService;
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;

@RequestMapping("checkIn")
@Slf4j
public abstract class CheckInController<A extends CheckInService> implements CheckInAPI<CheckInForm, CheckInVo> {

    protected static final String MEDIA_BOOKING_APPLICATION_JSON_PATCH = "application/json-patch+json";

    protected abstract A getCheckInService();
    
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewCheckIn(@RequestBody(required = false) CheckInForm form) throws CheckInException {
        log.debug("Requesting to create new checkIn");
        if(form != null) {
            String id = this.getCheckInService().createCheckIn(form);
            log.debug("Responding with identifier of newly created new checkIn");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("CheckInForm is null");
        throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }
    
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingCheckIn(@PathVariable String id, @RequestBody(required = false) CheckInForm form) throws CheckInException {
        log.debug("Requesting to update all attributes of existing checkIn");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                this.getCheckInService().updateCheckIn(id, form);
                log.debug("Responding with successful updation of attributes for existing checkIn");
                return;
            }
            log.debug("CheckInForm is null");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_ID_EMPTY.getValue());
        throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }
    
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingCheckIn(@PathVariable String id) throws CheckInException {
        log.debug("Requesting to soft delete checkIn");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            this.getCheckInService().deleteCheckIn(id);
            log.debug("Responding with successful deletion of existing checkIn");
            return;
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_ID_EMPTY.getValue());
        throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }
    
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_BOOKING_APPLICATION_JSON_PATCH)
    public void patchExistingCheckIn(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws CheckInException {
        log.debug("Requesting to patch of checkIn attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                this.getCheckInService().applyPatchOnCheckIn(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing checkIn");
                return;
            }
            log.debug("checkIn patch document is null");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_ID_EMPTY.getValue());
        throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }
    
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<CheckInVo> getAllCheckInNaturallyOrdered() {
        log.debug("Requesting all available checkIns by their natural orders");
        Set<CheckInVo> naturallyOrderedCheckIns = this.getCheckInService().retrieveAllByNaturalOrdering();
        log.debug("Responding with all available checkIns by their natural orders");
        return naturallyOrderedCheckIns;
    }
    
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("primaryFilter")
    public List<CheckInVo> getAllCheckInsByFilters(@RequestParam(required = false) String accountId,
                                                   @RequestParam(required = false) String categoryId,
                                                   @RequestParam(required = false) String timestamp) throws CheckInException {
        log.debug("Requesting all available checkIns with given filters");
        boolean emptyAccountId = !StringUtils.hasText(StringUtils.trimWhitespace(accountId));
        boolean emptyCategoryId = !StringUtils.hasText(StringUtils.trimWhitespace(categoryId));
        boolean emptyTimestamp = !StringUtils.hasText(StringUtils.trimWhitespace(timestamp));
        if(!emptyAccountId || !emptyCategoryId || !emptyTimestamp) {
            Optional<String> optAccountId = emptyAccountId ? Optional.empty() : Optional.of(accountId);
            Optional<String> optCategoryId = emptyCategoryId ? Optional.empty() : Optional.of(categoryId);
            Optional<String> optTimestamp = emptyTimestamp ? Optional.empty() : Optional.of(timestamp);
            List<CheckInVo> matchedByFilter = this.getCheckInService().retrieveAllMatchingDetailsByCriteria(optTimestamp, optAccountId, optCategoryId);
            log.debug("Responding with all available checkIns with given filters");
            return matchedByFilter;
        }
        log.debug("checkIn filters are empty");
        throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @ResponseStatus(HttpStatus.OK)
    @GetMapping("sequence/{sequence}")
    public List<CheckInVo> getAllCheckInsBySequenceOnDate(@PathVariable String sequence, @RequestParam(required = false) String date) throws CheckInException {
        List<CheckInVo> matchedBySequenceAndDate = new ArrayList<>();
        log.debug("Requesting all available checkIns with given sequence and date");
        if(StringUtils.hasText(StringUtils.trimWhitespace(sequence)) && StringUtils.hasText(StringUtils.trimWhitespace(date))) {
            matchedBySequenceAndDate = (List<CheckInVo>) this.getCheckInService().retrieveAllMatchingDetailsByCriteria(sequence, date);
            log.debug("Responding with all available checkIns with given sequence and date");
        } else if(StringUtils.isEmpty(StringUtils.trimWhitespace(sequence))) {
            log.debug("checkIn sequence is empty");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "sequence", sequence });
        } else if(StringUtils.isEmpty(StringUtils.trimWhitespace(date))) {
            log.debug("checkIn date is empty");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "date", date });
        }
        return matchedBySequenceAndDate;
    }
    
    /*@ResponseStatus(HttpStatus.OK)
    @GetMapping("categoryid/{categoryId}")
    public List<CheckInVo> getAllCheckInsByCategoryId(@PathVariable String categoryId, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws CheckInException {
        List<CheckInVo> matchedByCategoryIds = new ArrayList<>();
        log.debug("Requesting all available checkInes with given categoryId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(categoryId)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            matchedByCategoryIds = service.retrieveAllMatchingDetailsByCategoryId(categoryId, Optional.empty());
            log.debug("Responding with all available checkInes with given categoryId");
            return matchedByCategoryIds;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(categoryId)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                matchedByCategoryIds = service.retrieveAllMatchingDetailsByCategoryId(categoryId, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing checkIn details with given categoryId having fields cascaded to given level");
                return matchedByCategoryIds;
            } catch (NumberFormatException e) {
                log.debug(CheckInMessageTemplate.MSG_TEMPLATE_BOOKING_CASCADE_LEVEL_EMPTY.getValue());
                throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug("checkIn categoryId is empty");
        throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "categoryId", categoryId });
    }*/
    
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public CheckInVo getCheckInDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws CheckInException {
        CheckInVo checkInDetails = null;
        log.debug("Requesting all details of checkIn by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            checkInDetails = this.getCheckInService().retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing checkIn details by id");
            return checkInDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                checkInDetails = this.getCheckInService().retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing checkIn details by id wth fields cascaded to given level");
                return checkInDetails;
            } catch (NumberFormatException e) {
                log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_CASCADE_LEVEL_EMPTY.getValue());
                throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_ID_EMPTY.getValue());
        throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }
}
