package com.teenthofabud.restaurant.solution.reservation.engagement.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingException;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingVo;
import com.teenthofabud.restaurant.solution.reservation.booking.service.BookingService;
import com.teenthofabud.restaurant.solution.reservation.engagement.visitor.EngagementForm2DocumentAssigner;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryVo;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class EngagementForm2DocumentConverter<T extends EngagementDocument> implements Converter<EngagementForm, Optional<? extends EngagementDocument>> {

    private List<String> fieldsToEscape;
    private BookingService bookingService;

    @Value("#{'${res.reservation.engagement.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setBookingService(BookingService bookingService) {
        this.bookingService = bookingService;
    }

    @Override
    public Optional<? extends EngagementDocument> convert(EngagementForm form) {
        EngagementForm2DocumentAssigner engagementForm2DocumentAssigner = new EngagementForm2DocumentAssigner(form, fieldsToEscape);
        Optional<? extends EngagementDocument> optionalEngagementDocument = Optional.empty();
        if(!fieldsToEscape.contains("bookingId")) {
            try {
                BookingVo bookingVo = bookingService.retrieveDetailsById(form.getBookingId(), Optional.of(TOABCascadeLevel.TWO));
                log.debug("Retrieved {} for bookingId: {}", bookingVo, form.getBookingId());
                CategoryVo categoryVo = bookingVo.getCategory();
                switch (categoryVo.getName()) {
                    case "Dine In":
                        optionalEngagementDocument = Optional.of(new DineInEngagementDocument());
                        break;
                    case "Take Away":
                        optionalEngagementDocument = Optional.of(new TakeAwayEngagementDocument());
                        break;
                    case "Delivery":
                        optionalEngagementDocument = Optional.of(new DeliveryEngagementDocument());
                        break;
                    default:
                        String msg = "Category not supported";
                        log.error(msg + ": {}", categoryVo.getName());
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, msg, new Object[] { categoryVo.getName() });
                }
            } catch (BookingException e) {
                String msg = "Unable to retrieve booking details for bookingId: " + form.getBookingId();
                log.error(msg, e);
                throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, msg,
                        new Object[] { "Failed to retrieve booking details for " + form.getBookingId() + " : " + e.getMessage() });
            }
        }
        if(optionalEngagementDocument.isPresent()) {
            EngagementDocument document = optionalEngagementDocument.get();
            if(!fieldsToEscape.contains("tokenNumber")) {
                document.setTokenNumber(form.getTokenNumber());
            }
            document.assign(engagementForm2DocumentAssigner);
            document.setActive(Boolean.TRUE);
        }
        log.debug("Converting {} to {}", form, optionalEngagementDocument);
        return optionalEngagementDocument;
    }

}
