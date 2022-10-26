package com.teenthofabud.restaurant.solution.engagement.checkin.service;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public interface ReservationService<T extends ReservationForm, U extends ReservationVo> {

    public List<U> retrieveAllMatchingReservationDetailsByCriteria(Optional<String> optionalDate,
                                                                Optional<String> optionalTime) throws CheckInException;

}
