package com.teenthofabud.restaurant.solution.encounter.delivery.controller.impl;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.encounter.delivery.controller.DeliveryAPI;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryForm;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryVo;
import com.teenthofabud.restaurant.solution.encounter.delivery.service.impl.DeliveryServiceImpl;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingException;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingMessageTemplate;
import com.teenthofabud.restaurant.solution.encounter.constants.EncounterErrorCode;
import io.swagger.v3.oas.annotations.Operation;
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
public class DeliveryController extends DeliveryAPI implements ApplicationContextAware {

    private ApplicationContext applicationContext;

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }

    @Override
    public DeliveryServiceImpl getMeetingService() {
        return applicationContext.getBean(DeliveryServiceImpl.class);
    }

    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    @Override
    public CreatedVo postNewMeeting(@io.swagger.v3.oas.annotations.parameters.RequestBody(required = false,
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE,
            schema = @Schema(implementation = DeliveryForm.class)) })
                                        @RequestBody(required = false) DeliveryForm form) throws MeetingException {
        log.debug("Requesting to create new delivery");
        if (form != null) {
            String id = this.getMeetingService().createMeeting(form);
            log.debug("Responding with identifier of newly created new delivery");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        } else {
            log.debug("DeliveryForm is null");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED,
                    new Object[]{"form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED});
        }
    }

    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Override
    public void putExistingMeeting(@PathVariable String id,
                                   @io.swagger.v3.oas.annotations.parameters.RequestBody(required = false,
                                           content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE,
                                                   schema = @Schema(implementation = DeliveryForm.class)) })
                                   @RequestBody(required = false) DeliveryForm form) throws MeetingException {
        log.debug("Requesting to update all attributes of existing delivery");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                this.getMeetingService().updateMeeting(id, form);
                log.debug("Responding with successful updation of attributes for existing delivery");
                return;
            }
            log.debug("DeliveryForm is null");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_ID_EMPTY.getValue());
        throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    @Override
    public void deleteExistingMeeting(@PathVariable String id) throws MeetingException {
        log.debug("Requesting to soft delete delivery");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            this.getMeetingService().deleteMeeting(id);
            log.debug("Responding with successful deletion of existing delivery");
            return;
        }
        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_ID_EMPTY.getValue());
        throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Deliverys and their details ordered by accountId, sequence",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(
                            schema = @Schema(implementation = DeliveryVo.class)))})})
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    @Override
    public Set<DeliveryVo> getAllMeetingNaturallyOrdered() {
        log.debug("Requesting all available deliveries by their natural orders");
        Set<DeliveryVo> naturallyOrderedMeetings = this.getMeetingService().retrieveAllByNaturalOrdering();
        log.debug("Responding with all available deliveries by their natural orders");
        return naturallyOrderedMeetings;
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Deliveries and their details by accountId, sequence and notes",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(
                            schema = @Schema(implementation = DeliveryVo.class)))})})
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("primaryFilter")
    @Override
    public List<DeliveryVo> getAllMeetingsByFilters(@RequestParam(required = false) String accountId,
                                                  @RequestParam(required = false) String sequence) throws MeetingException {
        log.debug("Requesting all available deliveries with given filters");
        boolean emptyAccountId = !StringUtils.hasText(StringUtils.trimWhitespace(accountId));
        boolean emptySequence = !StringUtils.hasText(StringUtils.trimWhitespace(sequence));
        if(!emptyAccountId || !emptySequence ) {
            Optional<String> optAccountId = emptyAccountId ? Optional.empty() : Optional.of(accountId);
            Optional<String> optSequence = emptySequence ? Optional.empty() : Optional.of(sequence);
            List<DeliveryVo> matchedByFilter = this.getMeetingService().retrieveAllMatchingDetailsByCriteria(optAccountId, optSequence);
            log.debug("Responding with all available deliveries with given filters");
            return matchedByFilter;
        }
        log.debug("delivery filters are empty");
        throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve Delivery and its details by id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = DeliveryVo.class))})})
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    @Override
    public DeliveryVo getMeetingDetailsById(@PathVariable String id, @RequestParam(required = false) String cascadeUntilLevel) throws MeetingException {
        DeliveryVo walkInDetails = null;
        log.debug("Requesting all details of walkIn by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            walkInDetails = this.getMeetingService().retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing walkIn details by id");
            return walkInDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                int cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                walkInDetails = this.getMeetingService().retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing walkIn details by id wth fields cascaded to given level");
                return walkInDetails;
            } catch (NumberFormatException e) {
                log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_CASCADE_LEVEL_EMPTY.getValue());
                throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_ID_EMPTY.getValue());
        throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve Delivery and its details by sequence and date",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema =
                    @Schema(implementation = DeliveryVo.class))})})
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("sequence/{sequence}")
    @Override
    public DeliveryVo getMeetingBySequenceOnDate(@PathVariable String sequence,
                                                 @RequestParam(required = false) String date) throws MeetingException {
        DeliveryVo matchedBySequenceAndDate = new DeliveryVo();
        log.debug("Requesting all available deliveries with given sequence and date");
        if(StringUtils.hasText(StringUtils.trimWhitespace(sequence)) && StringUtils.hasText(StringUtils.trimWhitespace(date))) {
            matchedBySequenceAndDate = this.getMeetingService().retrieveMatchingDetailsBySequenceOnDate(sequence, date);
            log.debug("Responding with all available deliveries with given sequence and date");
        } else if(StringUtils.isEmpty(StringUtils.trimWhitespace(sequence))) {
            log.debug("delivery sequence is empty");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "sequence", sequence });
        } else if(StringUtils.isEmpty(StringUtils.trimWhitespace(date))) {
            log.debug("delivery date is empty");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "date", date });
        }
        return matchedBySequenceAndDate;
    }

    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_BOOKING_APPLICATION_JSON_PATCH)
    @Override
    public void patchExistingMeeting(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList) throws MeetingException {
        log.debug("Requesting to patch of pickUp attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                this.getMeetingService().applyPatchOnMeeting(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing pickUp");
                return;
            }
            log.debug("pickUp patch document is null");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(MeetingMessageTemplate.MSG_TEMPLATE_MEETING_ID_EMPTY.getValue());
        throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Delivery details by orderId")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available deliveries and their details that match the provided orderId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = DeliveryVo.class))) }),
            @ApiResponse(responseCode = "400", description = "delivery search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No deliveries available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("secondaryFilter")
    @Override
    public List<DeliveryVo> getAllDeliveryByDeliveryFilters(String orderId) throws MeetingException {
        log.debug("Requesting all available deliveries with given filters");
        boolean emptyOrderId = !StringUtils.hasText(StringUtils.trimWhitespace(orderId));
        if(!emptyOrderId) {
            Optional<String> optOrderId = Optional.of(orderId);
            List<DeliveryVo> matchedByFilter = this.getMeetingService().retrieveAllMatchingDeliveryDetailsByCriteria(optOrderId);
            log.debug("Responding with all available deliveries with given filters");
            return matchedByFilter;
        }else {
            log.debug("deliveries filters are empty");
            throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[]{"filters"});
        }
    }
}