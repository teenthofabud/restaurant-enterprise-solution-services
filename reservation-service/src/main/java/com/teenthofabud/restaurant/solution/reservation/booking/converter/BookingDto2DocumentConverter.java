package com.teenthofabud.restaurant.solution.reservation.booking.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingDocument;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class BookingDto2DocumentConverter implements ComparativePatchConverter<BookingDto, BookingDocument> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 4;

    private List<String> fieldsToEscape;
    private String bookingTimeFormat;

    @Value("#{'${res.reservation.booking.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Value("${res.reservation.booking.timestamp}")
    public void setBookingTimeFormat(String bookingTimeFormat) {
        this.bookingTimeFormat = bookingTimeFormat;
    }

    @Override
    public void compareAndMap(BookingDto dto, BookingDocument actualDocument) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;

        Optional<String> optCategoryId = dto.getCategoryId();
        if(!fieldsToEscape.contains("categoryId") && optCategoryId.isPresent()) {
            actualDocument.setCategoryId(optCategoryId.get());
            changeSW[i++] = true;
            log.debug("BookingDto.categoryId is valid");
        }

        Optional<String> optTimestamp = dto.getTimestamp();
        if(!fieldsToEscape.contains("timestamp") && optTimestamp.isPresent()) {
            LocalDateTime timestamp = LocalDateTime.parse(optTimestamp.get(), DateTimeFormatter.ofPattern(bookingTimeFormat));
            actualDocument.setTimestamp(timestamp);
            changeSW[i++] = true;
            log.debug("BookingDto.timestamp is valid");
        }

        Optional<String> optNoOfPerson = dto.getNoOfPerson();
        if(!fieldsToEscape.contains("noOfPerson") && optNoOfPerson.isPresent()) {
            actualDocument.setNoOfPerson(Integer.parseInt(optNoOfPerson.get()));
            changeSW[i++] = true;
            log.debug("BookingDto.noOfPerson is valid");
        }

        Optional<String> optAccountId = dto.getAccountId();
        if(!fieldsToEscape.contains("accountId") && optAccountId.isPresent()) {
            actualDocument.setAccountId(optAccountId.get());
            changeSW[i++] = true;
            log.debug("BookingDto.accountId is valid");
        }

        /*Optional<String> optEndedOn = dto.getEndedOn();
        if(!fieldsToEscape.contains("endedOn") && optEndedOn.isPresent()) {
            LocalDateTime endedOn = LocalDateTime.parse(optEndedOn.get(), DateTimeFormatter.ofPattern(endedOnFormat));
            actualDocument.setEndedOn(endedOn);
            changeSW[i++] = true;
            log.debug("BookingDto.endedOn is valid");
        }*/

        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent()) {
            actualDocument.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("BookingDto.active is valid");
        }

        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided BookingDto attributes are valid");
            actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided BookingDto attributes are valid");
    }

}
