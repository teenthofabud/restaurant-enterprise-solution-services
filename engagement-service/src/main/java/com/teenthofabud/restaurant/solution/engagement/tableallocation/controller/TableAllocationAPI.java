package com.teenthofabud.restaurant.solution.engagement.tableallocation.controller;

import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationException;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationForm;
import com.teenthofabud.restaurant.solution.engagement.tableallocation.data.TableAllocationVo;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.parameters.RequestBody;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.Set;


@Tag(name = "TableAllocation API", description = "Manage TableAllocations for CheckIns and their lifecycle")
public interface TableAllocationAPI {

    static final String MEDIA_BOOKING_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Operation(summary = "Create new TableAllocation details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created TableAllocation",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "TableAllocation attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "TableAllocation already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No TableAllocation attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new TableAllocation",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public CreatedVo postNewTableAllocation(@RequestBody(required = false, content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE,
            schema = @Schema(implementation = TableAllocationForm.class ))}) TableAllocationForm form) throws TableAllocationException;

    @Operation(summary = "Update TableAllocation details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of TableAllocation",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "TableAllocation attribute's value is invalid/TableAllocation is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No TableAllocation found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "TableAllocation already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update TableAllocation details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public void putExistingTableAllocation(@Parameter(in = ParameterIn.PATH, name = "id", description = "TableAllocation id") String id,
                                           @RequestBody(required = false, content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE,
            schema = @Schema(implementation = TableAllocationForm.class ))}) TableAllocationForm form) throws TableAllocationException ;

    @Operation(summary = "Soft delete TableAllocation by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted TableAllocation",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "TableAllocation id is invalid/TableAllocation is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No TableAllocation found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No TableAllocation attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete TableAllocation",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public void deleteExistingTableAllocation(@Parameter(in = ParameterIn.PATH, name = "id", description = "TableAllocation id")  String id) throws TableAllocationException ;

    @Operation(summary = "Patch TableAllocation attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of TableAllocation with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "TableAllocation attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No TableAllocation found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No TableAllocation attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of TableAllocation with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public void patchExistingTableAllocation(@Parameter(in = ParameterIn.PATH, name = "id", description = "TableAllocation id")  String id, @RequestBody(required = false,
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema =
            @Schema(implementation = PatchOperationForm.class))) }) List<PatchOperationForm> dtoList) throws TableAllocationException ;

    @Operation(summary = "Get all TableAllocation details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available TableAllocations and their details ordered by accountId, sequence",
                    content = {
                    @Content(
                            mediaType = MediaType.APPLICATION_JSON_VALUE,
                            array = @ArraySchema(
                                    schema = @Schema(
                                            implementation = TableAllocationVo.class
                                    )
                            ))
            })
    })
    public Set<TableAllocationVo> getAllTableAllocationNaturallyOrdered() ;

    @Operation(summary = "Get all TableAllocation details by accountId, sequence, notes")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available TableAllocations and their details ordered by accountId, sequence",
                    content = {
                            @Content(
                                    mediaType = MediaType.APPLICATION_JSON_VALUE,
                                    array = @ArraySchema(
                                            schema = @Schema(
                                                    implementation = TableAllocationVo.class
                                            )
                                    )
                            )
                    }
            ),
            @ApiResponse(responseCode = "400", description = "TableAllocation search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No TableAllocations available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public List<TableAllocationVo> getAllTableAllocationsByFilters(@Parameter(in = ParameterIn.QUERY, name = "accountId", description = "TableAllocation made with accountId") String accountId,
                                           @Parameter(in = ParameterIn.QUERY, name = "sequence", description = "TableAllocation sequence number for a day") String sequence,
                                           @Parameter(in = ParameterIn.QUERY, name = "notes", description = "TableAllocation has notes") String notes) throws TableAllocationException ;

    @Operation(summary = "Get TableAllocation details by checkInId")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available TableAllocations and their details ordered by checkInId",
                    content = {
                            @Content(
                                    mediaType = MediaType.APPLICATION_JSON_VALUE,
                                    array = @ArraySchema(
                                            schema = @Schema(
                                                    implementation = TableAllocationVo.class
                                            )
                                    )
                            )
                    }
            ),
            @ApiResponse(responseCode = "400", description = "TableAllocation id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No TableAllocations available with the given checkInId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public List<TableAllocationVo> getTableAllocationByCheckInId(@Parameter(in = ParameterIn.PATH, name = "checkInId", description = "checkInId having tables allocated") String checkInId) throws TableAllocationException;

    @Operation(summary = "Get TableAllocation details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of TableAllocation that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = TableAllocationVo.class)) }),
            @ApiResponse(responseCode = "400", description = "TableAllocation id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No TableAllocation found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    public TableAllocationVo getTableAllocationDetailsById(@Parameter(in = ParameterIn.PATH, name = "id", description = "TableAllocation id") String id,
    @Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body") String cascadeUntilLevel) throws TableAllocationException ;

}
