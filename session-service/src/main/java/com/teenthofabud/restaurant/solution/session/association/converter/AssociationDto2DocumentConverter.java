package com.teenthofabud.restaurant.solution.session.association.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.restaurant.solution.session.association.data.AssociationDocument;
import com.teenthofabud.restaurant.solution.session.association.data.AssociationDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Component
@Slf4j
public class AssociationDto2DocumentConverter implements ComparativePatchConverter<AssociationDto, AssociationDocument> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 4;

    private List<String> fieldsToEscape;
    //private String endedOnFormat;

    @Value("#{'${res.settings.discount.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    /*@Value("${res.session.association.endedon.format}")
    public void setEndedOnFormat(String endedOnFormat) {
        this.endedOnFormat = endedOnFormat;
    }*/

    @Override
    public void compareAndMap(AssociationDto dto, AssociationDocument actualDocument) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;

        Optional<String> optExperienceId = dto.getExperienceId();
        if(!fieldsToEscape.contains("experienceId") && optExperienceId.isPresent()) {
            actualDocument.setExperienceId(optExperienceId.get());
            changeSW[i++] = true;
            log.debug("AssociationDto.experienceId is valid");
        }

        Optional<String> optTableId = dto.getTableId();
        if(!fieldsToEscape.contains("tableId") && optTableId.isPresent()) {
            actualDocument.setTableId(optTableId.get());
            changeSW[i++] = true;
            log.debug("AssociationDto.tableId is valid");
        }

        Optional<String> optAccountId = dto.getAccountId();
        if(!fieldsToEscape.contains("accountId") && optAccountId.isPresent()) {
            actualDocument.setAccountId(optAccountId.get());
            changeSW[i++] = true;
            log.debug("AssociationDto.accountId is valid");
        }

        /*Optional<String> optEndedOn = dto.getEndedOn();
        if(!fieldsToEscape.contains("endedOn") && optEndedOn.isPresent()) {
            LocalDateTime endedOn = LocalDateTime.parse(optEndedOn.get(), DateTimeFormatter.ofPattern(endedOnFormat));
            actualDocument.setEndedOn(endedOn);
            changeSW[i++] = true;
            log.debug("AssociationDto.endedOn is valid");
        }*/

        Optional<String> optActive = dto.getActive();
        if(!fieldsToEscape.contains("active") && optActive.isPresent()) {
            actualDocument.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("AssociationDto.active is valid");
        }

        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided AssociationDto attributes are valid");
            actualDocument.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided AssociationDto attributes are valid");
    }

}
