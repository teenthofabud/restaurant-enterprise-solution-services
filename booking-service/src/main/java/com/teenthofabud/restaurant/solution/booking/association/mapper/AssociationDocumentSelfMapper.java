package com.teenthofabud.restaurant.solution.booking.association.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationDocument;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class AssociationDocumentSelfMapper implements SingleChannelMapper<AssociationDocument> {

    @Override
    public Optional<AssociationDocument> compareAndMap(AssociationDocument source, AssociationDocument target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source AssociationDocument.id is valid");
        }
        if(source.getExperienceId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getExperienceId())) && source.getExperienceId().compareTo(target.getExperienceId()) != 0) {
            target.setExperienceId(source.getExperienceId());
            changeSW = true;
            log.debug("Source AssociationDocument.experienceId is valid");
        }
        if(source.getTableId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getTableId())) && source.getTableId().compareTo(target.getTableId()) != 0) {
            target.setTableId(source.getTableId());
            changeSW = true;
            log.debug("Source AssociationDocument.tableId is valid");
        }
        if(source.getAccountId() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getAccountId())) && source.getAccountId().compareTo(target.getAccountId()) != 0) {
            target.setAccountId(source.getAccountId());
            changeSW = true;
            log.debug("Source AssociationDocument.accountId is valid");
        }
        if(changeSW) {
            log.debug("All provided AssociationDocument attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided AssociationDocument attributes are valid");
            return Optional.empty();
        }
    }
}
