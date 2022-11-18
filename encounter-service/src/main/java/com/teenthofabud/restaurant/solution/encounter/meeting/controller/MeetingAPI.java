package com.teenthofabud.restaurant.solution.encounter.meeting.controller;

import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryForm;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryVo;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingException;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingForm;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingVo;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpForm;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpVo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.DiscriminatorMapping;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.Set;

public interface MeetingAPI<T extends MeetingForm, U extends MeetingVo> {

    @Operation(summary = "Create new Meeting details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Meeting",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Meeting attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Meeting already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Meeting attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Meeting",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public CreatedVo postNewMeeting(@RequestBody(required = false, content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE,
            schema = @Schema(oneOf = { PickUpForm.class, DeliveryForm.class }, discriminatorMapping = {
                    @DiscriminatorMapping( value = "PICK_UP", schema = PickUpForm.class ),
                    @DiscriminatorMapping( value = "DELIVERY", schema = DeliveryForm.class )
            }, discriminatorProperty = "type"))}) T form) throws MeetingException;

    @Operation(summary = "Update Meeting details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Meeting",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Meeting attribute's value is invalid/Meeting is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Meeting found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Meeting already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Meeting details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public void putExistingMeeting(@Parameter(in = ParameterIn.PATH, name = "id", description = "Meeting id") String id, @RequestBody(required = false,
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE,
            schema = @Schema(oneOf = { PickUpForm.class, DeliveryForm.class }, discriminatorMapping = {
                    @DiscriminatorMapping( value = "PICK_UP", schema = PickUpForm.class ),
                    @DiscriminatorMapping( value = "DELIVERY", schema = DeliveryForm.class )
            }, discriminatorProperty = "type"))}) T form)
            throws MeetingException ;

    @Operation(summary = "Soft delete Meeting by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Meeting",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Meeting id is invalid/Meeting is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Meeting found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Meeting attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Meeting",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public void deleteExistingMeeting(@Parameter(in = ParameterIn.PATH, name = "id", description = "Meeting id")  String id) throws MeetingException ;

    @Operation(summary = "Patch Meeting attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Meeting with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Meeting attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Meeting found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Meeting attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Meeting with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public void patchExistingMeeting(@Parameter(in = ParameterIn.PATH, name = "id", description = "Meeting id")  String id, @RequestBody(required = false,
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema =
            @Schema(implementation = PatchOperationForm.class))) }) List<PatchOperationForm> dtoList) throws MeetingException ;

    @Operation(summary = "Get all Meeting details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Meetings and their details ordered by accountId, sequence",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(
                            schema = @Schema(oneOf = { PickUpVo.class, DeliveryVo.class }, discriminatorMapping = {
                                    @DiscriminatorMapping( value = "PICK_UP", schema = PickUpVo.class ),
                                    @DiscriminatorMapping( value = "DELIVERY", schema = DeliveryVo.class )
                            }, discriminatorProperty = "type"))) })
    })
    public Set<U> getAllMeetingNaturallyOrdered() ;

    @Operation(summary = "Get all Meeting details by accountId, sequence, notes")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Meetings and their details that match the provided accountId, sequence, notes",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(
                            schema = @Schema(oneOf = { PickUpVo.class, DeliveryVo.class }, discriminatorMapping = {
                                    @DiscriminatorMapping( value = "PICK_UP", schema = PickUpVo.class ),
                                    @DiscriminatorMapping( value = "DELIVERY", schema = DeliveryVo.class )
                            }, discriminatorProperty = "type"))) }),
            @ApiResponse(responseCode = "400", description = "Meeting search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Meetings available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public List<U> getAllMeetingsByFilters(@Parameter(in = ParameterIn.QUERY, name = "accountId", description = "Meeting made with accountId") String accountId,
                                           @Parameter(in = ParameterIn.QUERY, name = "sequence", description = "Meeting sequence number for a day") String sequence) 
            throws MeetingException ;

    @Operation(summary = "Get all Meeting details by sequence on date")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Meetinges and their details that match the given category id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema =
                    @Schema(oneOf = { PickUpVo.class, DeliveryVo.class }, discriminatorMapping = {
                            @DiscriminatorMapping( value = "PICK_UP", schema = PickUpVo.class ),
                            @DiscriminatorMapping( value = "DELIVERY", schema = DeliveryVo.class )
                    }, discriminatorProperty = "type"))) }),
            @ApiResponse(responseCode = "400", description = "Meeting id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Meetinges available with the given category id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public List<U> getAllMeetingsBySequenceOnDate(@Parameter(in = ParameterIn.PATH, name = "sequence",
            description = "sequence number for a day") String sequence,
                                                          @Parameter(in = ParameterIn.QUERY, name = "date",
            description = "expected date the sequence was generated")  String date) throws MeetingException;

    @Operation(summary = "Get Meeting details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Meeting that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(oneOf = { PickUpVo.class, DeliveryVo.class },
                            discriminatorMapping = {
                                    @DiscriminatorMapping( value = "PICK_UP", schema = PickUpVo.class ),
                                    @DiscriminatorMapping( value = "DELIVERY", schema = DeliveryVo.class )
                            }, discriminatorProperty = "type")) }),
            @ApiResponse(responseCode = "400", description = "Meeting id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Meeting found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public U getMeetingDetailsById(@Parameter(in = ParameterIn.PATH, name = "id", description = "Meeting id") String id,
    @Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws MeetingException ;

}
