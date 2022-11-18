package com.teenthofabud.restaurant.solution.encounter.pickup.service;

import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingException;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpForm;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public interface PickUpService<T extends PickUpForm, U extends PickUpVo> {

    public List<U> retrieveAllMatchingPickUpDetailsByCriteria(Optional<String> optionalName,
            Optional<String> optionalPhoneNo) throws MeetingException;

}
