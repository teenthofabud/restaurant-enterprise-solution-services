package com.teenthofabud.restaurant.solution.encounter.pickup.controller.impl;

import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.encounter.meeting.controller.impl.MeetingController;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingException;
import com.teenthofabud.restaurant.solution.encounter.pickup.controller.PickUpAPI;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpForm;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpVo;
import com.teenthofabud.restaurant.solution.encounter.pickup.service.impl.PickUpServiceImpl;
import constants.EncounterErrorCode;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("pickUp")
@Slf4j
@Tag(name = "PickUp API", description = "Manage PickUps and their details")
public class PickUpController extends MeetingController<PickUpServiceImpl> implements ApplicationContextAware, PickUpAPI<PickUpForm, PickUpVo> {

    private ApplicationContext applicationContext;

    protected PickUpServiceImpl getMeetingService() {
        return applicationContext.getBean(PickUpServiceImpl.class);
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }

    @Operation(summary = "Get all PickUp details by name, phoneNumber")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available pickUps and their details that match the provided name, phoneNumber",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = PickUpVo.class))) }),
            @ApiResponse(responseCode = "400", description = "pickUp search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No pickUps available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("secondaryFilter")
    @Override
    public List<PickUpVo> getAllPickUpsByPickUpFilters(String name, String phoneNumber) throws MeetingException {
        log.debug("Requesting all available pickUps with given filters");
        boolean emptyName = !StringUtils.hasText(StringUtils.trimWhitespace(name));
        boolean emptyPhoneNumber = !StringUtils.hasText(StringUtils.trimWhitespace(phoneNumber));
        if(!emptyName || !emptyPhoneNumber) {
            Optional<String> optName = emptyName ? Optional.empty() : Optional.of(name);
            Optional<String> optPhoneNumber = emptyPhoneNumber ? Optional.empty() : Optional.of(phoneNumber);
            List<PickUpVo> matchedByFilter = this.getMeetingService().retrieveAllMatchingPickUpDetailsByCriteria(optName, optPhoneNumber);
            log.debug("Responding with all available pickUps with given filters");
            return matchedByFilter;
        }
        log.debug("pickUps filters are empty");
        throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }
}
