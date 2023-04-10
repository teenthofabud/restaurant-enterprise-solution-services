package com.teenthofabud.restaurant.solution.encounter.delivery.service;

import com.teenthofabud.restaurant.solution.encounter.delivery.converter.DeliveryDto2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.delivery.converter.DeliveryEntity2VoConverter;
import com.teenthofabud.restaurant.solution.encounter.delivery.converter.DeliveryForm2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryForm;
import com.teenthofabud.restaurant.solution.encounter.delivery.data.DeliveryVo;
import com.teenthofabud.restaurant.solution.encounter.delivery.mapper.DeliveryEntitySelfMapper;
import com.teenthofabud.restaurant.solution.encounter.delivery.mapper.DeliveryForm2EntityMapper;
import com.teenthofabud.restaurant.solution.encounter.delivery.repository.DeliveryRepository;
import com.teenthofabud.restaurant.solution.encounter.delivery.validator.DeliveryDtoValidator;
import com.teenthofabud.restaurant.solution.encounter.delivery.validator.DeliveryFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.encounter.delivery.validator.DeliveryFormValidator;
import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingException;
import com.teenthofabud.restaurant.solution.encounter.meeting.service.MeetingService;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public interface DeliveryService extends MeetingService<DeliveryForm, DeliveryVo, DeliveryFormValidator, DeliveryFormRelaxedValidator,
        DeliveryDtoValidator, DeliveryRepository, DeliveryEntitySelfMapper, DeliveryForm2EntityMapper, DeliveryForm2EntityConverter,
        DeliveryEntity2VoConverter, DeliveryDto2EntityConverter> {

    public List<DeliveryVo> retrieveAllMatchingDeliveryDetailsByCriteria(Optional<String> optionalOrderId)
            throws MeetingException;

}
