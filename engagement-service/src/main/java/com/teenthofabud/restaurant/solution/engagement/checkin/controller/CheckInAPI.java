package com.teenthofabud.restaurant.solution.engagement.checkin.controller;

import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.service.CheckInService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;

import java.util.List;
import java.util.Set;

@RequestMapping("checkIn")
public abstract class CheckInAPI<T extends CheckInForm, U extends CheckInVo, A extends CheckInService> {

    protected static final String MEDIA_BOOKING_APPLICATION_JSON_PATCH = "application/json-patch+json";

    public abstract A getCheckInService();

    @Operation(summary = "Create new CheckIn details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created CheckIn",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "CheckIn attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "CheckIn already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No CheckIn attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new CheckIn",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public abstract CreatedVo postNewCheckIn(T form) throws CheckInException ;

    @Operation(summary = "Update CheckIn details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of CheckIn",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "CheckIn attribute's value is invalid/CheckIn is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No CheckIn found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "CheckIn already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update CheckIn details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public abstract void putExistingCheckIn(@Parameter(in = ParameterIn.PATH, name = "id", description = "CheckIn id") String id, T form)
            throws CheckInException ;

    @Operation(summary = "Soft delete CheckIn by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted CheckIn",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "CheckIn id is invalid/CheckIn is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No CheckIn found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No CheckIn attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete CheckIn",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public abstract void deleteExistingCheckIn(@Parameter(in = ParameterIn.PATH, name = "id", description = "CheckIn id")  String id) throws CheckInException ;

    @Operation(summary = "Patch CheckIn attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of CheckIn with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "CheckIn attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No CheckIn found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No CheckIn attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of CheckIn with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public abstract void patchExistingCheckIn(@Parameter(in = ParameterIn.PATH, name = "id", description = "CheckIn id")  String id, @RequestBody(required = false,
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema =
            @Schema(implementation = PatchOperationForm.class))) }) List<PatchOperationForm> dtoList) throws CheckInException ;

    @Operation(summary = "Get all CheckIn details")
    public abstract Set<U> getAllCheckInNaturallyOrdered() ;

    @Operation(summary = "Get all CheckIn details by accountId, sequence, notes")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "400", description = "CheckIn search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No CheckIns available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public abstract List<U> getAllCheckInsByFilters(@Parameter(in = ParameterIn.QUERY, name = "accountId", description = "CheckIn made with accountId") String accountId,
                                           @Parameter(in = ParameterIn.QUERY, name = "sequence", description = "CheckIn sequence number for a day") String sequence,
                                           @Parameter(in = ParameterIn.QUERY, name = "notes", description = "CheckIn has notes") String notes) throws CheckInException ;

    @Operation(summary = "Get CheckIn details by sequence on date")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "400", description = "CheckIn id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No CheckInes available with the given sequence",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public abstract U getCheckInBySequenceOnDate(@Parameter(in = ParameterIn.PATH, name = "sequence", description = "sequence number for a day") String sequence,
                                                          @Parameter(in = ParameterIn.QUERY, name = "date", description = "expected date the sequence was generated")  String date) throws CheckInException;

    @Operation(summary = "Get CheckIn details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "400", description = "CheckIn id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No CheckIn found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public abstract U getCheckInDetailsById(@Parameter(in = ParameterIn.PATH, name = "id", description = "CheckIn id") String id,
    @Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body") String cascadeUntilLevel) throws CheckInException ;

}
