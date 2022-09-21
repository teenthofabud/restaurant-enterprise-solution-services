package com.teenthofabud.restaurant.solution.engagement.booking.mapper;

import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.restaurant.solution.engagement.booking.data.BookingDocument;
import com.teenthofabud.restaurant.solution.engagement.booking.data.BookingForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class BookingForm2DocumentMapper implements DualChannelMapper<BookingDocument, BookingForm> {

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
    public Optional<BookingDocument> compareAndMap(BookingDocument actualDocument, BookingForm form) {
        BookingDocument expectedDocument = new BookingDocument();
        boolean changeSW = false;
        // direct copy
        expectedDocument.setId(actualDocument.getId());
        log.debug("Directly copying BookingDocument.id: {} from actualDocument to expectedDocument", actualDocument.getId());
        expectedDocument.setCreatedOn(actualDocument.getCreatedOn());
        log.debug("Directly copying BookingDocument.createdOn: {} from actualDocument to expectedDocument", actualDocument.getCreatedOn());
        expectedDocument.setActive(actualDocument.getActive());
        log.debug("Directly copying BookingDocument.active: {} from actualDocument to expectedDocument", actualDocument.getActive());
        // comparative copy

        if(!fieldsToEscape.contains("categoryId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getCategoryId()))
                && form.getCategoryId().compareTo(actualDocument.getCategoryId()) != 0) {
            expectedDocument.setCategoryId(form.getCategoryId());
            changeSW = true;
            log.debug("BookingForm.categoryId: {} is different as BookingDocument.categoryId: {}", form.getCategoryId(), actualDocument.getCategoryId());
        } else {
            expectedDocument.setCategoryId(actualDocument.getCategoryId());
            log.debug("BookingForm.categoryId: is unchanged");
        }

        /*if(!fieldsToEscape.contains("tableId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getTableId()))
                && form.getTableId().compareTo(actualDocument.getTableId()) != 0) {
            expectedDocument.setTableId(form.getTableId());
            changeSW = true;
            log.debug("BookingForm.tableId: {} is different as BookingDocument.tableId: {}", form.getTableId(), actualDocument.getTableId());
        } else {
            expectedDocument.setTableId(actualDocument.getTableId());
            log.debug("BookingForm.tableId: is unchanged");
        }*/

        if(!fieldsToEscape.contains("timestamp") && StringUtils.hasText(StringUtils.trimWhitespace(form.getTimestamp()))) {
            DateTimeFormatter dtf = DateTimeFormatter.ofPattern(bookingTimeFormat);
            if(form.getTimestamp().compareTo(dtf.format(actualDocument.getTimestamp())) != 0) {
                expectedDocument.setTimestamp(LocalDateTime.parse(form.getTimestamp(), dtf));
                changeSW = true;
                log.debug("BookingForm.timestamp: {} is different as BookingDocument.timestamp: {}", form.getTimestamp(), actualDocument.getTimestamp());
            } else {
                expectedDocument.setTimestamp(actualDocument.getTimestamp());
                log.debug("BookingForm.timestamp: is unchanged");
            }
        } else {
            expectedDocument.setTimestamp(actualDocument.getTimestamp());
            log.debug("BookingForm.timestamp: is unchanged");
        }

        /*if(!fieldsToEscape.contains("noOfPerson") && form.getNoOfPerson() != null && form.getNoOfPerson().compareTo(actualDocument.getNoOfPerson()) != 0) {
            expectedDocument.setNoOfPerson(form.getNoOfPerson());
            changeSW = true;
            log.debug("BookingForm.noOfPerson: {} is different as BookingDocument.noOfPerson: {}", form.getNoOfPerson(), actualDocument.getNoOfPerson());
        } else {
            expectedDocument.setNoOfPerson(actualDocument.getNoOfPerson());
            log.debug("BookingForm.noOfPerson: is unchanged");
        }*/

        if(!fieldsToEscape.contains("accountId") && StringUtils.hasText(StringUtils.trimWhitespace(form.getAccountId()))
                && form.getAccountId().compareTo(actualDocument.getAccountId()) != 0) {
            expectedDocument.setAccountId(form.getAccountId());
            changeSW = true;
            log.debug("BookingForm.accountId: {} is different as BookingDocument.accountId: {}", form.getAccountId(), actualDocument.getAccountId());
        } else {
            expectedDocument.setAccountId(actualDocument.getAccountId());
            log.debug("BookingForm.accountId: is unchanged");
        }

        return changeSW ? Optional.of(expectedDocument) : Optional.empty();
    }

}
