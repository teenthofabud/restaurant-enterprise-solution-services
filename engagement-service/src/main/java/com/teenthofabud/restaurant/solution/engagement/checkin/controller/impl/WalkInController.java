package com.teenthofabud.restaurant.solution.engagement.checkin.controller.impl;

import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.controller.WalkInAPI;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.impl.WalkInServiceImpl;
import com.teenthofabud.restaurant.solution.engagement.constants.EngagementErrorCode;
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
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("walkIn")
@Slf4j
@Tag(name = "WalkIn API", description = "Manage WalkIns and their details")
public class WalkInController extends CheckInController<WalkInServiceImpl> implements ApplicationContextAware, WalkInAPI<WalkInForm, WalkInVo> {

    private ApplicationContext applicationContext;

    protected WalkInServiceImpl getCheckInService() {
        return applicationContext.getBean(WalkInServiceImpl.class);
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }

    @Operation(summary = "Get all WalkIn details by name, phoneNumber, emailId")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available walkIns and their details that match the provided name, phoneNumber, emailId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = WalkInVo.class))) }),
            @ApiResponse(responseCode = "400", description = "walkIn search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No walkIns available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("secondaryFilter")
    @Override
    public List<WalkInVo> getAllWalkInsByWalkInFilters(String name, String phoneNumber, String emailId) throws CheckInException {
        log.debug("Requesting all available walkIns with given filters");
        boolean emptyName = !StringUtils.hasText(StringUtils.trimWhitespace(name));
        boolean emptyPhoneNumber = !StringUtils.hasText(StringUtils.trimWhitespace(phoneNumber));
        boolean emptyEmailId = !StringUtils.hasText(StringUtils.trimWhitespace(emailId));
        if(!emptyName || !emptyPhoneNumber || !emptyEmailId) {
            Optional<String> optName = emptyName ? Optional.empty() : Optional.of(name);
            Optional<String> optPhoneNumber = emptyPhoneNumber ? Optional.empty() : Optional.of(phoneNumber);
            Optional<String> optEmailId = emptyEmailId ? Optional.empty() : Optional.of(emailId);
            List<WalkInVo> matchedByFilter = this.getCheckInService().retrieveAllMatchingWalkInDetailsByCriteria(optEmailId, optName, optPhoneNumber);
            log.debug("Responding with all available walkIns with given filters");
            return matchedByFilter;
        }
        log.debug("walkIns filters are empty");
        throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }
}
