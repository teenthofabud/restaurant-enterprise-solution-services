/*
package com.teenthofabud.restaurant.solution.encounter.meeting.controller.impl;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.restaurant.solution.encounter.meeting.controller.MeetingAPI;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingException;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingForm;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingMessageTemplate;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingVo;
import com.teenthofabud.restaurant.solution.encounter.meeting.service.MeetingService;
import constants.EncounterErrorCode;
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

@RequestMapping("meeting")
@Slf4j
public abstract class MeetingController<A extends MeetingService> implements MeetingAPI<MeetingForm, MeetingVo> {

    protected static final String MEDIA_MEETING_APPLICATION_JSON_PATCH = "application/json-patch+json";

    protected abstract A getMeetingService();
    
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewMeeting(@RequestBody(required = false) MeetingForm form) throws MeetingException {
        log.debug("Requesting to create new meeting");
        if(form != null) {
            String id = this.getMeetingService().createMeeting(form);
            log.debug("Responding with identifier of newly created new meeting");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("MeetingForm is null");
        throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }
    
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingMeeting(@PathVariable String id, @RequestBody(required = false) MeetingForm form) throws MeetingException {
        log.debug("Requesting to update all attributes of existing meeting");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                this.getMeetingService().updateMeeting(id, form);
                log.debug("Responding with successful updation of attributes for existing meeting");
                return;
            }
            log.debug("MeetingForm is null");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_ID_EMPTY.getValue());
        throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }
    
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingMeeting(@PathVariable String id) throws MeetingException {
        log.debug("Requesting to soft delete meeting");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            this.getMeetingService().deleteMeeting(id);
            log.debug("Responding with successful deletion of existing meeting");
            return;
        }
        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_ID_EMPTY.getValue());
        throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }
    
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_MEETING_APPLICATION_JSON_PATCH)
    public void patchExistingMeeting(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws MeetingException {
        log.debug("Requesting to patch of meeting attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                this.getMeetingService().applyPatchOnMeeting(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing meeting");
                return;
            }
            log.debug("meeting patch document is null");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_ID_EMPTY.getValue());
        throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }
    
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<MeetingVo> getAllMeetingNaturallyOrdered() {
        log.debug("Requesting all available meetings by their natural orders");
        Set<MeetingVo> naturallyOrderedMeetings = this.getMeetingService().retrieveAllByNaturalOrdering();
        log.debug("Responding with all available meetings by their natural orders");
        return naturallyOrderedMeetings;
    }
    
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("primaryFilter")
    public List<MeetingVo> getAllMeetingsByFilters(@RequestParam(required = false) String accountId,
                                                   @RequestParam(required = false) String timestamp) throws MeetingException {
        log.debug("Requesting all available meetings with given filters");
        boolean emptyAccountId = !StringUtils.hasText(StringUtils.trimWhitespace(accountId));
        boolean emptyTimestamp = !StringUtils.hasText(StringUtils.trimWhitespace(timestamp));
        if(!emptyAccountId || !emptyTimestamp) {
            Optional<String> optAccountId = emptyAccountId ? Optional.empty() : Optional.of(accountId);
            Optional<String> optTimestamp = emptyTimestamp ? Optional.empty() : Optional.of(timestamp);
            List<MeetingVo> matchedByFilter = this.getMeetingService().retrieveAllMatchingDetailsByCriteria(optTimestamp, optAccountId);
            log.debug("Responding with all available meetings with given filters");
            return matchedByFilter;
        }
        log.debug("meeting filters are empty");
        throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @ResponseStatus(HttpStatus.OK)
    @GetMapping("sequence/{sequence}")
    public List<MeetingVo> getAllMeetingsBySequenceOnDate(@PathVariable String sequence, @RequestParam(required = false) String date) throws MeetingException {
        List<MeetingVo> matchedBySequenceAndDate = new ArrayList<>();
        log.debug("Requesting all available meetings with given sequence and date");
        if(StringUtils.hasText(StringUtils.trimWhitespace(sequence)) && StringUtils.hasText(StringUtils.trimWhitespace(date))) {
            matchedBySequenceAndDate = (List<MeetingVo>) this.getMeetingService().retrieveAllMatchingDetailsByCriteria(sequence, date);
            log.debug("Responding with all available meetings with given sequence and date");
        } else if(StringUtils.isEmpty(StringUtils.trimWhitespace(sequence))) {
            log.debug("meeting sequence is empty");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "sequence", sequence });
        } else if(StringUtils.isEmpty(StringUtils.trimWhitespace(date))) {
            log.debug("meeting date is empty");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "date", date });
        }
        return matchedBySequenceAndDate;
    }

    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public MeetingVo getMeetingDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws MeetingException {
        MeetingVo meetingDetails = null;
        log.debug("Requesting all details of meeting by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            meetingDetails = this.getMeetingService().retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing meeting details by id");
            return meetingDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                meetingDetails = this.getMeetingService().retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing meeting details by id wth fields cascaded to given level");
                return meetingDetails;
            } catch (NumberFormatException e) {
                log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_CASCADE_LEVEL_EMPTY.getValue());
                throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_ID_EMPTY.getValue());
        throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }
}
*/
