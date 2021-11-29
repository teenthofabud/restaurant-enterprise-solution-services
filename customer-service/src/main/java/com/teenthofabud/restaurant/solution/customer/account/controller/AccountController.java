package com.teenthofabud.restaurant.solution.customer.account.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.CreatedVo;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountException;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountForm;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountMessageTemplate;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountVo;
import com.teenthofabud.restaurant.solution.customer.account.service.AccountService;
import com.teenthofabud.restaurant.solution.customer.error.CustomerErrorCode;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@RestController
@RequestMapping("account")
@Slf4j
@Tag(name = "Account API", description = "Manage Accounts and their details")
public class AccountController {

    private static final String MEDIA_CUST_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(AccountService service) {
        this.service = service;
    }

    private AccountService service;

    @Operation(summary = "Create new Account details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created Account",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = CreatedVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Account attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Account already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Account attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new Account",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public CreatedVo postNewAccount(@RequestBody(required = false) AccountForm form) throws AccountException {
        log.debug("Requesting to create new account");
        if(form != null) {
            String id = service.createAccount(form);
            log.debug("Responding with identifier of newly created new account");
            CreatedVo createdVo = new CreatedVo();
            createdVo.setId(id);
            return createdVo;
        }
        log.debug("AccountForm is null");
        throw new AccountException(CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update Account details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of Account",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Account attribute's value is invalid/Account is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Account found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "Account already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update Account details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public void putExistingAccount(@PathVariable String id, @RequestBody(required = false) AccountForm form) throws AccountException {
        log.debug("Requesting to update all attributes of existing account");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(form != null) {
                service.updateAccount(id, form);
                log.debug("Responding with successful updation of attributes for existing account");
                return;
            }
            log.debug("AccountForm is null");
            throw new AccountException(CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(AccountMessageTemplate.MSG_TEMPLATE_ACCOUNT_ID_EMPTY.getValue());
        throw new AccountException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete Account by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted Account",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Account id is invalid/Account is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Account found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Account attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete Account",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public void deleteExistingAccount(@PathVariable String id) throws AccountException {
        log.debug("Requesting to soft delete account");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            service.deleteAccount(id);
            log.debug("Responding with successful deletion of existing account");
            return;
        }
        log.debug(AccountMessageTemplate.MSG_TEMPLATE_ACCOUNT_ID_EMPTY.getValue());
        throw new AccountException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch Account attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of Account with the given value",
                    content = { @Content(schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "Account attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Account found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No Account attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of Account with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_CUST_APPLICATION_JSON_PATCH)
    public void patchExistingAccount(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws AccountException {
        log.debug("Requesting to patch of account attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            if(dtoList != null) {
                service.applyPatchOnAccount(id, dtoList);
                log.debug("Responding with successful patch of attributes for existing account");
                return;
            }
            log.debug("account patch document is null");
            throw new AccountException(CustomerErrorCode.CUST_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug(AccountMessageTemplate.MSG_TEMPLATE_ACCOUNT_ID_EMPTY.getValue());
        throw new AccountException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all Account details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Accounts and their details ordered by first name, last name",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = AccountVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<AccountVo> getAllAccountNaturallyOrdered() {
        log.debug("Requesting all available accounts by their natural orders");
        Set<AccountVo> naturallyOrderedAccounts = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available accounts by their natural orders");
        return naturallyOrderedAccounts;
    }

    @Operation(summary = "Get all Account details by gender id", hidden = true)
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Accounts and their details that match the given gender id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = AccountVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Gender id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Accounts available with the given gender id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("genderid/{genderId}")
    public List<AccountVo> getAllAccountsByGenderId(@PathVariable String genderId) throws AccountException {
        log.debug("Requesting all available accounts with given genderId");
        if(StringUtils.hasText(StringUtils.trimWhitespace(genderId))) {
            List<AccountVo> matchedByGenderIds = service.retrieveAllMatchingDetailsByGenderId(genderId);
            log.debug("Responding with all available accounts with given genderId");
            return matchedByGenderIds;
        }
        log.debug("account genderId is empty");
        throw new AccountException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "genderId", genderId });
    }

    @Operation(summary = "Get all Account details by first name, last name, phone number, email id, date of birth")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available Accounts and their details that match the provided first name, last name, phone number, email id, date of birth",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = AccountVo.class))) }),
            @ApiResponse(responseCode = "400", description = "Account search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Accounts available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("filter")
    public List<AccountVo> getAllAccountsByFilters(@RequestParam(required = false) String firstName, @RequestParam(required = false) String lastName,
                                                 @RequestParam(required = false) String dateOfBirth, @RequestParam(required = false) String phoneNumber,
                                                 @RequestParam(required = false) String emailId) throws AccountException {
        log.debug("Requesting all available accounts with given filters");
        boolean emptyFirstName = !StringUtils.hasText(StringUtils.trimWhitespace(firstName));
        boolean emptyLastName = !StringUtils.hasText(StringUtils.trimWhitespace(lastName));
        boolean emptyDateOfBirth = !StringUtils.hasText(StringUtils.trimWhitespace(dateOfBirth));
        boolean emptyPhoneNumber = !StringUtils.hasText(StringUtils.trimWhitespace(phoneNumber));
        boolean emptyEmailId = !StringUtils.hasText(StringUtils.trimWhitespace(emailId));
        if(!emptyFirstName || !emptyLastName || !emptyDateOfBirth || !emptyPhoneNumber || !emptyEmailId) {
            Optional<String> optAccountName = emptyFirstName ? Optional.empty() : Optional.of(firstName);
            Optional<String> optLastName = emptyLastName ? Optional.empty() : Optional.of(lastName);
            Optional<String> optDateOfBirth = emptyDateOfBirth ? Optional.empty() : Optional.of(dateOfBirth);
            Optional<String> optPhoneNumber = emptyPhoneNumber ? Optional.empty() : Optional.of(phoneNumber);
            Optional<String> optEmailId = emptyEmailId ? Optional.empty() : Optional.of(emailId);
            List<AccountVo> matchedByFilter = service.retrieveAllMatchingDetailsByCriteria(
                    optAccountName, optLastName, optPhoneNumber, optEmailId, optDateOfBirth);
            log.debug("Responding with all available accounts with given filters");
            return matchedByFilter;
        }
        log.debug("account filters are empty");
        throw new AccountException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }

    @Operation(summary = "Get Account details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of Account that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = AccountVo.class)) }),
            @ApiResponse(responseCode = "400", description = "Account id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No Account found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public AccountVo getAccountDetailsById(@PathVariable String id, @RequestParam(required = false)
    @io.swagger.v3.oas.annotations.Parameter(in = ParameterIn.QUERY, description = "levels of nested fields to be unfolded within the response body")
            String cascadeUntilLevel) throws AccountException {
        AccountVo accountDetails = null;
        log.debug("Requesting all details of account by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.isEmpty(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            accountDetails = service.retrieveDetailsById(id, Optional.empty());
            log.debug("Responding with successful retrieval of existing account details by id");
            return accountDetails;
        } else if(StringUtils.hasText(StringUtils.trimWhitespace(id)) && StringUtils.hasText(StringUtils.trimWhitespace(cascadeUntilLevel))) {
            try {
                Integer cascadeLevelCode = Integer.parseInt(cascadeUntilLevel);
                if(cascadeLevelCode < 0) {
                    throw new NumberFormatException();
                }
                log.debug("Requested with cascade level code: {}", cascadeLevelCode);
                Optional<TOABCascadeLevel> optCascadeLevel = TOABCascadeLevel.findByLevelCode(cascadeUntilLevel);
                accountDetails = service.retrieveDetailsById(id, optCascadeLevel);
                log.debug("Responding with successful retrieval of existing account details by id wth fields cascaded to given level");
                return accountDetails;
            } catch (NumberFormatException e) {
                log.debug(AccountMessageTemplate.MSG_TEMPLATE_ACCOUNT_CASCADE_LEVEL_EMPTY.getValue());
                throw new AccountException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "cascadeUntilLevel", cascadeUntilLevel });
            }
        }
        log.debug(AccountMessageTemplate.MSG_TEMPLATE_ACCOUNT_ID_EMPTY.getValue());
        throw new AccountException(CustomerErrorCode.CUST_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
