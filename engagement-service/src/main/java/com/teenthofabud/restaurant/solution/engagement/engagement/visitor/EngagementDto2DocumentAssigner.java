package com.teenthofabud.restaurant.solution.engagement.engagement.visitor;

import com.teenthofabud.restaurant.solution.checkin.engagement.data.*;
import com.teenthofabud.restaurant.solution.engagement.engagement.data.DeliveryEngagementDocument;
import com.teenthofabud.restaurant.solution.engagement.engagement.data.DineInEngagementDocument;
import com.teenthofabud.restaurant.solution.engagement.engagement.data.EngagementDto;
import com.teenthofabud.restaurant.solution.engagement.engagement.data.TakeAwayEngagementDocument;
import lombok.extern.slf4j.Slf4j;

import java.util.List;
import java.util.Optional;

@Slf4j
public class EngagementDto2DocumentAssigner {

    private EngagementDto dto;
    private List<String> fieldsToEscape;

    public EngagementDto2DocumentAssigner(EngagementDto dto, List<String> fieldsToEscape) {
        this.dto = dto;
        this.fieldsToEscape = fieldsToEscape;
    }

    public Integer assign(DineInEngagementDocument document) {
        int i = 0 ;
        Optional<String>  optTableId = dto.getTableId();
        if(!fieldsToEscape.contains("date") && optTableId.isPresent()) {
            document.setTableId(optTableId.get());
            i++;
            log.debug("EngagementDto.date is valid");
        }
        Optional<String>  optNoOfPersons = dto.getNoOfPersons();
        if(!fieldsToEscape.contains("noOfPersons") && optNoOfPersons.isPresent()) {
            document.setNoOfPersons(Integer.parseInt(optNoOfPersons.get()));
            i++;
            log.debug("EngagementDto.noOfPersons is valid");
        }
        return i;
    }

    public Integer assign(TakeAwayEngagementDocument document) {
        int i = 0 ;
        Optional<String>  optInstructions = dto.getInstructions();
        if(!fieldsToEscape.contains("instructions") && optInstructions.isPresent()) {
            document.setInstructions(optInstructions.get());
            i++;
            log.debug("EngagementDto.instructions is valid");
        }
        return i;
    }

    public Integer assign(DeliveryEngagementDocument document) {
        int i = 0 ;
        Optional<String>  optExtRef = dto.getExtRef();
        if(!fieldsToEscape.contains("extRef") && optExtRef.isPresent()) {
            document.setExtRef(dto.getExtRef().get());
            i++;
            log.debug("EngagementDto.extRef is valid");
        }
        return i;
    }

}
