package com.teenthofabud.restaurant.solution.encounter.pickup.service;

import com.teenthofabud.restaurant.solution.encounter.meeting.data.MeetingException;
import com.teenthofabud.restaurant.solution.encounter.meeting.service.MeetingService;
import com.teenthofabud.restaurant.solution.encounter.pickup.converter.PickUpDto2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.pickup.converter.PickUpEntity2VoConverter;
import com.teenthofabud.restaurant.solution.encounter.pickup.converter.PickUpForm2EntityConverter;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpForm;
import com.teenthofabud.restaurant.solution.encounter.pickup.data.PickUpVo;
import com.teenthofabud.restaurant.solution.encounter.pickup.mapper.PickUpEntitySelfMapper;
import com.teenthofabud.restaurant.solution.encounter.pickup.mapper.PickUpForm2EntityMapper;
import com.teenthofabud.restaurant.solution.encounter.pickup.repository.PickUpRepository;
import com.teenthofabud.restaurant.solution.encounter.pickup.validator.PickUpDtoValidator;
import com.teenthofabud.restaurant.solution.encounter.pickup.validator.PickUpFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.encounter.pickup.validator.PickUpFormValidator;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.RequestMapping;

import java.util.List;
import java.util.Optional;

@Service
public interface PickUpService extends MeetingService<PickUpForm, PickUpVo, PickUpFormValidator, PickUpFormRelaxedValidator,
        PickUpDtoValidator, PickUpRepository, PickUpEntitySelfMapper, PickUpForm2EntityMapper, PickUpForm2EntityConverter,
        PickUpEntity2VoConverter, PickUpDto2EntityConverter> {

    public List<PickUpVo> retrieveAllMatchingPickUpDetailsByCriteria(Optional<String> optionalName, Optional<String> optionalPhoneNumber)
            throws MeetingException;

}