package com.teenthofabud.restaurant.solution.encounter.delivery.controller.impl;

import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.restaurant.solution.encounter.delivery.controller.DeliveryAPI;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryForm;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryVo;
import com.teenthofabud.restaurant.solution.encounter.delivery.service.impl.DeliveryServiceImpl;
import com.teenthofabud.restaurant.solution.encounter.meeting.controller.impl.MeetingController;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingException;
import constants.EncounterErrorCode;
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

@RestController
@RequestMapping("delivery")
@Slf4j
@Tag(name = "Delivery API", description = "Manage Delivery and their details")
public class DeliveryController extends MeetingController<DeliveryServiceImpl>
        implements ApplicationContextAware, DeliveryAPI<DeliveryForm, DeliveryVo> {

    private ApplicationContext applicationContext;

    protected DeliveryServiceImpl getMeetingService() {
        return applicationContext.getBean(DeliveryServiceImpl.class);
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }

    @Operation(summary = "Get all Delivery details by orderId")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available deliveries and their details that match the provided orderId",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = DeliveryVo.class))) }),
            @ApiResponse(responseCode = "400", description = "delivery search filters are invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No deliveries available by the given filters",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("secondaryFilter")
    @Override
    public List<DeliveryVo> getAllDeliveryByDeliveryFilters(String orderId) throws MeetingException {
        log.debug("Requesting all available deliveries with given filters");
        boolean emptyOrderId = !StringUtils.hasText(StringUtils.trimWhitespace(orderId));
        if(!emptyOrderId) {
            Optional<String> optOrderId = emptyOrderId ? Optional.empty() : Optional.of(orderId);
            List<DeliveryVo> matchedByFilter = this.getMeetingService().retrieveAllMatchingDeliveryDetailsByCriteria(optOrderId);
            log.debug("Responding with all available deliveries with given filters");
            return matchedByFilter;
        }
        log.debug("deliveries filters are empty");
        throw new MeetingException(EncounterErrorCode.ENCOUNTER_ATTRIBUTE_INVALID, new Object[] { "filters" });
    }
}
