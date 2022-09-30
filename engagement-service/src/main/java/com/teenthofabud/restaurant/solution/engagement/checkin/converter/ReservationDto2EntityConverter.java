package com.teenthofabud.restaurant.solution.engagement.checkin.converter;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class ReservationDto2EntityConverter extends CheckInDto2EntityConverter {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 10;

    private List<String> fieldsToEscape;
    private String reservationDateFormat;
    private String reservationTimeFormat;

    @Value("${res.engagement.checkIn.reservation.time.format}")
    public void setReservationTimeFormat(String reservationTimeFormat) {
        this.reservationTimeFormat = reservationTimeFormat;
    }

    @Value("${res.engagement.checkIn.reservation.date.format}")
    public void setReservationDateFormat(String reservationDateFormat) {
        this.reservationDateFormat = reservationDateFormat;
    }


    @Value("#{'${res.engagement.checkIn.reservation.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Override
    protected void compareAndMap(Optional<? extends CheckInDtoParameters> optionalCheckInDtoParameters, Optional<? extends CheckInEntity> optionalCheckInEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;
        ReservationDto dto = (ReservationDto) optionalCheckInDtoParameters.get();
        ReservationEntity actualEntity = (ReservationEntity) optionalCheckInEntity.get();

        Optional<String> optDate = dto.getDate();
        if(!fieldsToEscape.contains("date") && optDate.isPresent()) {
            actualEntity.setDate(LocalDate.parse(optDate.get(), DateTimeFormatter.ofPattern(reservationDateFormat)));
            changeSW[i++] = true;
            log.debug("ReservationDto.date is valid");
        }

        Optional<String> optTime = dto.getTime();
        if(!fieldsToEscape.contains("time") && optTime.isPresent()) {
            actualEntity.setTime(LocalTime.parse(optDate.get(), DateTimeFormatter.ofPattern(reservationTimeFormat)));
            changeSW[i++] = true;
            log.debug("ReservationDto.time is valid");
        }

        log.debug("Not all provided ReservationDto attributes are valid");
    }

}
