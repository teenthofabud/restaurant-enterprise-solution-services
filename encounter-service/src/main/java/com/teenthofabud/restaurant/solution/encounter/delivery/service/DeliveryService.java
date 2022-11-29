package com.teenthofabud.restaurant.solution.encounter.delivery.service;

import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryForm;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryVo;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingException;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public interface DeliveryService<T extends DeliveryForm, U extends DeliveryVo> {

    public List<U> retrieveAllMatchingDeliveryDetailsByCriteria(Optional<String> optionalOrderId)
            throws MeetingException;

}
