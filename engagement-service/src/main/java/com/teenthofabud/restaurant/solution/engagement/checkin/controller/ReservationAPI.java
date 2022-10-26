package com.teenthofabud.restaurant.solution.engagement.checkin.controller;

import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationVo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

public interface ReservationAPI<T extends CheckInForm, U extends CheckInVo> {

    @Operation(summary = "Get all Reservation details by date, time")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available CheckIns and their details that match the provided date, time",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = ReservationVo.class))) }),
            @ApiResponse(responseCode = "400", description = "CheckIn search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No CheckIns available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public List<U> getAllReservationsByReservationFilters(@RequestParam(required = false) String date, @RequestParam(required = false) String time) throws CheckInException ;

}
