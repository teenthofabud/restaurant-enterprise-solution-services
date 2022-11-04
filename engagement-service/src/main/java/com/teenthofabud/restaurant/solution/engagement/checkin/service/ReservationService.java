package com.teenthofabud.restaurant.solution.engagement.checkin.service;

import com.teenthofabud.restaurant.solution.engagement.checkin.converter.ReservationDto2EntityConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.ReservationEntity2VoConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.ReservationForm2EntityConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.CheckInException;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationForm;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.ReservationVo;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.ReservationEntitySelfMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.ReservationForm2EntityMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.ReservationRepository;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.ReservationDtoValidator;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.ReservationFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.ReservationFormValidator;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public interface ReservationService extends CheckInService<ReservationForm, ReservationVo, ReservationFormValidator, ReservationFormRelaxedValidator,
        ReservationDtoValidator, ReservationRepository, ReservationEntitySelfMapper, ReservationForm2EntityMapper, ReservationForm2EntityConverter,
        ReservationEntity2VoConverter, ReservationDto2EntityConverter> {

    public void setReservationTimeFormat(String reservationTimeFormat);

    public void setReservationDateFormat(String reservationDateFormat);

    public List<ReservationVo> retrieveAllMatchingReservationDetailsByCriteria(Optional<String> optionalDate, Optional<String> optionalTime) throws CheckInException;

}
