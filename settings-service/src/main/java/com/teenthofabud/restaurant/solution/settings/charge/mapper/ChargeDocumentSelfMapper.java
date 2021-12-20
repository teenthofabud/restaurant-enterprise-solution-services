package com.teenthofabud.restaurant.solution.settings.charge.mapper;

import com.teenthofabud.core.common.mapper.SingleChannelMapper;
import com.teenthofabud.restaurant.solution.settings.charge.data.ChargeDocument;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import java.util.Optional;

@Component
@Slf4j
public class ChargeDocumentSelfMapper implements SingleChannelMapper<ChargeDocument> {

    @Override
    public Optional<ChargeDocument> compareAndMap(ChargeDocument source, ChargeDocument target) {
        boolean changeSW = false;
        if(source.getId() != null && source.getId().compareTo(target.getId()) != 0) {
            target.setId(source.getId());
            changeSW = true;
            log.debug("Source ChargeDocument.id is valid");
        }
        if(source.getName() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getName())) && source.getName().compareTo(target.getName()) != 0) {
            target.setName(source.getName());
            changeSW = true;
            log.debug("Source ChargeDocument.name is valid");
        }
        if(source.getDescription() != null && StringUtils.hasText(StringUtils.trimWhitespace(source.getDescription())) && source.getDescription().compareTo(target.getDescription()) != 0) {
            target.setDescription(source.getDescription());
            changeSW = true;
            log.debug("Source ChargeDocument.description is valid");
        }
        if(source.getRate() != null && source.getRate() != null && source.getRate().compareTo(target.getRate()) != 0) {
            target.setRate(source.getRate());
            changeSW = true;
            log.debug("Source ChargeDocument.rate is valid");
        }
        if(changeSW) {
            log.debug("All provided ChargeDocument attributes are valid");
            return Optional.of(target);
        } else {
            log.debug("Not all provided ChargeDocument attributes are valid");
            return Optional.empty();
        }
    }
}
