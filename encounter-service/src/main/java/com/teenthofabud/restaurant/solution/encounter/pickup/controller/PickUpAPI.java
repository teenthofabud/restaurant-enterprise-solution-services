package com.teenthofabud.restaurant.solution.encounter.pickup.controller;

import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.encounter.meeting.controller.MeetingAPI;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingException;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingForm;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpForm;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpVo;
import com.teenthofabud.restaurant.solution.encounter.pickup.service.PickUpService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

@RequestMapping("pickUp")
@Tag(name = "PickUp API", description = "Manage PickUp and their details")
public abstract class PickUpAPI extends MeetingAPI<PickUpForm, PickUpVo, PickUpService> {

    @Operation(summary = "Get all PickUp details by name, phoneNumber")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Meetings and their details that match the provided name, phoneNumber",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = PickUpVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Meeting search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Meetings available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public abstract List<PickUpVo> getAllPickUpsByPickUpFilters(@RequestParam(required = false) String name,
                                                                  @RequestParam(required = false) String phoneNumber) throws MeetingException;
}
