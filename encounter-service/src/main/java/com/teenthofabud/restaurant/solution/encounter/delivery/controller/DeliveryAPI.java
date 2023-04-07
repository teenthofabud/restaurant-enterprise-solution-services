package com.teenthofabud.restaurant.solution.encounter.delivery.controller;

import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryForm;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryVo;
import com.teenthofabud.restaurant.solution.encounter.delivery.service.DeliveryService;
import com.teenthofabud.restaurant.solution.encounter.meeting.controller.MeetingAPI;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingException;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingForm;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingVo;
import com.teenthofabud.restaurant.solution.encounter.meeting.service.MeetingService;
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

@RequestMapping("delivery")
@Tag(name = "Delivery API", description = "Manage Delivery and their details")
public abstract class DeliveryAPI extends MeetingAPI<DeliveryForm, DeliveryVo, DeliveryService> {

    @Operation(summary = "Get all Delivery details by orderId")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Meetings and their details that match the provided orderId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = DeliveryVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Meeting search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Meetings available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public abstract List<DeliveryVo> getAllDeliveryByDeliveryFilters(@RequestParam(required = false) String orderId) throws MeetingException;

}
