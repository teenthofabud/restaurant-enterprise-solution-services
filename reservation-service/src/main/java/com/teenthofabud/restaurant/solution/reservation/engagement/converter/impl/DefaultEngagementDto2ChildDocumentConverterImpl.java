package com.teenthofabud.restaurant.solution.reservation.engagement.converter.impl;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingException;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingVo;
import com.teenthofabud.restaurant.solution.reservation.booking.service.BookingService;
import com.teenthofabud.restaurant.solution.reservation.engagement.converter.EngagementDto2ChildDocumentConverter;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import java.util.*;

@Component
@Slf4j
public class DefaultEngagementDto2ChildDocumentConverterImpl implements EngagementDto2ChildDocumentConverter<EngagementDto, EngagementDocument> {

    private BookingService bookingService;

    @Autowired
    public void setBookingService(BookingService bookingService) {
        this.bookingService = bookingService;
    }

    @Override
    public <T extends EngagementDocument> Optional<? extends EngagementDocument> convert(EngagementDto engagementDto) {
        Optional<? extends EngagementDocument> optionalEngagementDocument = Optional.empty();
        Optional<String> optionalBookingId = engagementDto.getBookingId();
        try {
            BookingVo bookingVo = bookingService.retrieveDetailsById(optionalBookingId.get(), Optional.of(TOABCascadeLevel.TWO));
            String categoryName = bookingVo.getCategory().getName();
            switch (categoryName) {
                case "Dine In":
                    DineInEngagementDocument dineInEngagementDocument = new DineInEngagementDocument();
                    dineInEngagementDocument.setTableId(engagementDto.getTableId().get());
                    optionalEngagementDocument = Optional.of(dineInEngagementDocument);
                    break;
                case "Take Away":
                    TakeAwayEngagementDocument takeAwayEngagementDocument = new TakeAwayEngagementDocument();
                    takeAwayEngagementDocument.setInstructions(engagementDto.getInstructions().get());
                    optionalEngagementDocument = Optional.of(takeAwayEngagementDocument);
                    break;
                case "Delivery":
                    DeliveryEngagementDocument deliveryEngagementDocument = new DeliveryEngagementDocument();
                    deliveryEngagementDocument.setExtRef(engagementDto.getExtRef().get());
                    optionalEngagementDocument = Optional.of(deliveryEngagementDocument);
                    break;
                default:
                    String msg = "Category not supported";
                    log.error(msg + ": {}", categoryName);
                    throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, msg, new Object[] { categoryName });
            }
        } catch (BookingException e) {
            String action = "repository retrieval by category";
            String msg = "Unable to perform " + action;
            log.error(msg, e);
            throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, msg, new Object[] { action + " failure: " + e.getMessage() });
        }
        return optionalEngagementDocument;
    }
}
