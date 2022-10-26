package com.teenthofabud.restaurant.solution.engagement.checkin.service;

import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public interface WalkInService<T extends WalkInForm, U extends WalkInVo> {

    public List<U> retrieveAllMatchingWalkInDetailsByCriteria(Optional<String> optionalName,
                                                                Optional<String> optionalPhoneNumber,
                                                                Optional<String> optionalEmailId) throws CheckInException;

}
