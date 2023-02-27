package com.teenthofabud.restaurant.solution.engagement.checkin.controller.impl;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.controller.WalkInAPI;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.impl.WalkInServiceImpl;
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@RestController
@Slf4j
public class WalkInController extends WalkInAPI implements ApplicationContextAware {

    private ApplicationContext applicationContext;
    @Override
    public WalkInServiceImpl getCheckInService() {
        return applicationContext.getBean(WalkInServiceImpl.class);
    }

    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    @Override
    public CreatedVo postNewCheckIn(@io.swagger.v3.oas.annotations.parameters.RequestBody(required = false, content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE,
            schema = @Schema(implementation = WalkInForm.class)) }) @RequestBody(required = false) WalkInForm form) throws CheckInException {
        log.debug("Requesting to create new walkIn");
        if(form != null) {
            String id = this.getCheckInService().createCheckIn(form);
            log.debug("Responding with identifier of newly created new walkIn");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("WalkInForm is null");
        throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Override
    public void putExistingCheckIn(@PathVariable String id, @io.swagger.v3.oas.annotations.parameters.RequestBody(required = false, content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE,
            schema = @Schema(implementation = WalkInForm.class)) }) @RequestBody(required = false) WalkInForm form) throws CheckInException {
        log.debug("Requesting to update all attributes of existing walkIn");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                this.getCheckInService().updateCheckIn(id, form);
                log.debug("Responding with successful updation of attributes for existing walkIn");
                return;
            }
            log.debug("WalkInForm is null");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_ID_EMPTY.getValue());
        throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    @Override
    public void deleteExistingCheckIn(@PathVariable String id) throws CheckInException {
        log.debug("Requesting to soft delete walkIn");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            this.getCheckInService().deleteCheckIn(id);
            log.debug("Responding with successful deletion of existing walkIn");
            return;
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_ID_EMPTY.getValue());
        throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_BOOKING_APPLICATION_JSON_PATCH)
    @Override
    public void patchExistingCheckIn(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList) throws CheckInException {
        log.debug("Requesting to patch of walkIn attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                this.getCheckInService().applyPatchOnCheckIn(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing walkIn");
                return;
            }
            log.debug("walkIn patch document is null");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_ID_EMPTY.getValue());
        throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available WalkIns and their details ordered by accountId, sequence",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(
                            schema = @Schema(implementation = WalkInVo.class)))})})
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    @Override
    public Set<WalkInVo> getAllCheckInNaturallyOrdered() {
        log.debug("Requesting all available walkIns by their natural orders");
        Set<WalkInVo> naturallyOrderedCheckIns = this.getCheckInService().retrieveAllByNaturalOrdering();
        log.debug("Responding with all available walkIns by their natural orders");
        return naturallyOrderedCheckIns;
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available WalkIns and their details by accountId, sequence and notes",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(
                            schema = @Schema(implementation = WalkInVo.class)))})})
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("primaryFilter")
    @Override
    public List<WalkInVo> getAllCheckInsByFilters(@RequestParam(required = false) String accountId, @RequestParam(required = false) String sequence,
                                                       @RequestParam(required = false) String notes) throws CheckInException {
        log.debug("Requesting all available walkIns with given filters");
        boolean emptyAccountId = !StringUtils.hasText(StringUtils.trimWhitespace(accountId));
        boolean emptySequence = !StringUtils.hasText(StringUtils.trimWhitespace(sequence));
        boolean emptyNotes = !StringUtils.hasText(StringUtils.trimWhitespace(notes));
        if(!emptyAccountId || !emptySequence || !emptyNotes) {
            Optional<String> optAccountId = emptyAccountId ? Optional.empty() : Optional.of(accountId);
            Optional<String> optSequence = emptySequence ? Optional.empty() : Optional.of(sequence);
            Optional<String> optNotes = emptyNotes ? Optional.empty() : Optional.of(notes);
            List<WalkInVo> matchedByFilter = this.getCheckInService().retrieveAllMatchingDetailsByCriteria(optAccountId, optSequence, optNotes);
            log.debug("Responding with all available walkIns with given filters");
            return matchedByFilter;
        }
        log.debug("walkIn filters are empty");
        throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve WalkIn and its details by sequence and date",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = WalkInVo.class))})})
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("sequence/{sequence}")
    @Override
    public WalkInVo getCheckInBySequenceOnDate(@PathVariable String sequence, @RequestParam(required = false) String date) throws CheckInException {
        WalkInVo matchedBySequenceAndDate = new WalkInVo();
        log.debug("Requesting all available walkIns with given sequence and date");
        if(StringUtils.hasText(StringUtils.trimWhitespace(sequence)) && StringUtils.hasText(StringUtils.trimWhitespace(date))) {
            matchedBySequenceAndDate = this.getCheckInService().retrieveMatchingDetailsByCriteria(sequence, date);
            log.debug("Responding with all available walkIns with given sequence and date");
        } else if(StringUtils.isEmpty(StringUtils.trimWhitespace(sequence))) {
            log.debug("walkIn sequence is empty");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "sequence", sequence });
        } else if(StringUtils.isEmpty(StringUtils.trimWhitespace(date))) {
            log.debug("walkIn date is empty");
            throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "date", date });
        }
        return matchedBySequenceAndDate;
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve WalkIn and its details by id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = WalkInVo.class))})})
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    @Override
    public WalkInVo getCheckInDetailsById(@PathVariable String id, @RequestParam(required = false) String cascadeUntilLevel) throws CheckInException {
        WalkInVo walkInDetails = null;
        log.debug("Requesting all details of walkIn by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            walkInDetails = this.getCheckInService().retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing walkIn details by id");
            return walkInDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                walkInDetails = this.getCheckInService().retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing walkIn details by id wth fields cascaded to given level");
                return walkInDetails;
            } catch (NumberFormatException e) {
                log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_CASCADE_LEVEL_EMPTY.getValue());
                throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(CheckInMessageTemplate.MSG_TEMPLATE_CHECKIN_ID_EMPTY.getValue());
        throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }

    @ResponseStatus(HttpStatus.OK)
    @GetMapping("secondaryFilter")
    @Override
    public List<WalkInVo> getAllWalkInsByWalkInFilters(@RequestParam(required = false) String name, @RequestParam(required = false) String phoneNumber,
                                                       @RequestParam(required = false) String emailId) throws CheckInException {
        log.debug("Requesting all available walkIns with given filters");
        boolean emptyName = !StringUtils.hasText(StringUtils.trimWhitespace(name));
        boolean emptyPhoneNumber = !StringUtils.hasText(StringUtils.trimWhitespace(phoneNumber));
        boolean emptyEmailId = !StringUtils.hasText(StringUtils.trimWhitespace(emailId));
        if(!emptyName || !emptyPhoneNumber || !emptyEmailId) {
            Optional<String> optName = emptyName ? Optional.empty() : Optional.of(name);
            Optional<String> optPhoneNumber = emptyPhoneNumber ? Optional.empty() : Optional.of(phoneNumber);
            Optional<String> optEmailId = emptyEmailId ? Optional.empty() : Optional.of(emailId);
            List<WalkInVo> matchedByFilter = this.getCheckInService().retrieveAllMatchingWalkInDetailsByCriteria(optName, optPhoneNumber, optEmailId);
            log.debug("Responding with all available walkIns with given filters");
            return matchedByFilter;
        }
        log.debug("walkIns filters are empty");
        throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }
}
