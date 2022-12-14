package com.teenthofabud.restaurant.solution.engagement.checkin.service;

import com.teenthofabud.restaurant.solution.engagement.checkin.converter.WalkInDto2EntityConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.WalkInEntity2VoConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.converter.WalkInForm2EntityConverter;
import com.teenthofabud.restaurant.solution.engagement.checkin.data.*;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.WalkInEntitySelfMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.mapper.WalkInForm2EntityMapper;
import com.teenthofabud.restaurant.solution.engagement.checkin.repository.WalkInRepository;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.WalkInDtoValidator;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.WalkInFormRelaxedValidator;
import com.teenthofabud.restaurant.solution.engagement.checkin.validator.WalkInFormValidator;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public interface WalkInService extends CheckInService<WalkInForm, WalkInVo, WalkInFormValidator, WalkInFormRelaxedValidator,
        WalkInDtoValidator, WalkInRepository, WalkInEntitySelfMapper, WalkInForm2EntityMapper, WalkInForm2EntityConverter,
        WalkInEntity2VoConverter, WalkInDto2EntityConverter> {

    public void setWalkInTimeFormat(String walkInTimeFormat);

    public List<WalkInVo> retrieveAllMatchingWalkInDetailsByName(String name) throws CheckInException;

    public List<WalkInVo> retrieveAllMatchingWalkInDetailsByCriteria(Optional<String> optionalName, Optional<String> optionalPhoneNumber, Optional<String> optionalEmailId) throws CheckInException;

}
