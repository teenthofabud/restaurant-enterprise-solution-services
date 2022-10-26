package com.teenthofabud.restaurant.solution.engagement.checkin.controller.impl;

import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.controller.ReservationAPI;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.impl.ReservationServiceImpl;
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
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@RestController
@RequestMapping("reservation")
@Slf4j
@Tag(name = "Reservation API", description = "Manage Reservations and their details")
public class ReservationController extends CheckInController<ReservationServiceImpl> implements ApplicationContextAware, ReservationAPI<ReservationForm, ReservationVo> {

    private ApplicationContext applicationContext;

    protected ReservationServiceImpl getCheckInService() {
        return applicationContext.getBean(ReservationServiceImpl.class);
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }

    @Operation(summary = "Get all Reservation details by date, time")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available reservations and their details that match the provided date, time",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = ReservationVo.class))) }),
            @ApiResponse(responseCode = "400", description = "reservation search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No reservations available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("secondaryFilter")
    @Override
    public List<ReservationVo> getAllReservationsByReservationFilters(String date, String time) throws CheckInException {
        log.debug("Requesting all available reservations with given filters");
        boolean emptyDate = !StringUtils.hasText(StringUtils.trimWhitespace(date));
        boolean emptyTime = !StringUtils.hasText(StringUtils.trimWhitespace(time));
        if(!emptyDate || !emptyTime) {
            Optional<String> optDate = emptyDate ? Optional.empty() : Optional.of(date);
            Optional<String> optTime = emptyTime ? Optional.empty() : Optional.of(time);
            List<ReservationVo> matchedByFilter = this.getCheckInService().retrieveAllMatchingReservationDetailsByCriteria(optDate, optTime);
            log.debug("Responding with all available reservations with given filters");
            return matchedByFilter;
        }
        log.debug("reservations filters are empty");
        throw new CheckInException(EngagementErrorCode.ENGAGEMENT_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }
}
