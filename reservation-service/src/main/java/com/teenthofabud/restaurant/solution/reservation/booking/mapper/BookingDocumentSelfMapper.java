package com.teenthofabud.restaurant.solution.reservation.booking.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.reservation.booking.data.BookingDocument;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class BookingDocumentSelfMapper implements SingleChannelMapper<BookingDocument> {

    @Override
    public Optional<BookingDocument> compareAndMap(BookingDocument source, BookingDocument target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source BookingDocument.id is valid");
        }
        if(source.getCategoryId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getCategoryId())) && source.getCategoryId().compareTo(target.getCategoryId()) != 0) {
            target.setCategoryId(source.getCategoryId());
            changeSW = true;
            log.debug("Source BookingDocument.categoryId is valid");
        }
        /*if(source.getTableId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getTableId())) && source.getTableId().compareTo(target.getTableId()) != 0) {
            target.setTableId(source.getTableId());
            changeSW = true;
            log.debug("Source BookingDocument.tableId is valid");
        }*/
        if(source.getTimestamp() != null && source.getTimestamp().compareTo(target.getTimestamp()) != 0) {
            target.setTimestamp(source.getTimestamp());
            changeSW = true;
            log.debug("Source BookingDocument.timestamp is valid");
        }
        /*if(source.getNoOfPerson() != null && source.getNoOfPerson().compareTo(target.getNoOfPerson()) != 0) {
            target.setNoOfPerson(source.getNoOfPerson());
            changeSW = true;
            log.debug("Source BookingDocument.noOfPerson is valid");
        }*/
        if(source.getAccountId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getAccountId())) && source.getAccountId().compareTo(target.getAccountId()) != 0) {
            target.setAccountId(source.getAccountId());
            changeSW = true;
            log.debug("Source BookingDocument.accountId is valid");
        }
        if(changeSW) {
            log.debug("All provided BookingDocument attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided BookingDocument attributes are valid");
            return Optional.empty();
        }
    }
}
