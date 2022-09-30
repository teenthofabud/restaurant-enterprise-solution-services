package com.teenthofabud.restaurant.solution.engagement.checkin.converter;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class ReservationEntity2VoConverter extends CheckInEntity2VoConverter {

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
    protected CheckInVoParameters convert(Optional<? extends CheckInEntity> optionalCheckInEntityChild) {
        ReservationEntity entity = (ReservationEntity) optionalCheckInEntityChild.get();
        ReservationVo vo = new ReservationVo();
        if(!fieldsToEscape.contains("date")) {
            vo.setDate(entity.getDate().format(DateTimeFormatter.ofPattern(reservationDateFormat)));
        }
        if(!fieldsToEscape.contains("time")) {
            vo.setTime(entity.getTime().format(DateTimeFormatter.ofPattern(reservationTimeFormat)));
        }
        log.debug("Converted {} to {} ", entity, vo);
        return vo;
    }

}
