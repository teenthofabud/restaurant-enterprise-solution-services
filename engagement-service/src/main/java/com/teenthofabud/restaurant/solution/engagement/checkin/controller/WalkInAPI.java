package com.teenthofabud.restaurant.solution.engagement.checkin.controller;

import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.WalkInForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.WalkInVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.WalkInService;
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

@RequestMapping("walkIn")
@Tag(name = "WalkIn API", description = "Manage WalkIns and their details")
public interface WalkInAPI extends CheckInAPI<WalkInForm, WalkInVo, WalkInService> {

    @Operation(summary = "Get all WalkIn details by name, phoneNumber, emailId")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available CheckIns and their details that match the provided name, phoneNumber, emailId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = WalkInVo.class))) }),
            @ApiResponse(responseCode = "400", description = "CheckIn search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No CheckIns available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public List<WalkInVo> getAllWalkInsByWalkInFilters(@RequestParam(required = false) String name,
                                                   @RequestParam(required = false) String phoneNumber,
                                                   @RequestParam(required = false) String emailId) throws CheckInException ;

}
