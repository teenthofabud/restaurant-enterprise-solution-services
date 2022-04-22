package com.teenthofabud.restaurant.solution.booking.engagement.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseDocument2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.booking.engagement.data.EngagementDocument;
import com.teenthofabud.restaurant.solution.booking.engagement.data.EngagementVo;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationVo;
import com.teenthofabud.restaurant.solution.booking.association.service.AssociationService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.scheduling.concurrent.CustomizableThreadFactory;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.*;

@Component
@Slf4j
public class EngagementDocument2VoConverter extends TOABBaseDocument2VoConverter<EngagementDocument, EngagementVo> implements Converter<EngagementDocument, EngagementVo> {

    private List<String> fieldsToEscape;
    private AssociationService associationService;


    @Value("#{'${res.session.engagement.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setAssociationService(AssociationService associationService) {
        this.associationService = associationService;
    }

    @Override
    public EngagementVo convert(EngagementDocument document) {
        EngagementVo vo = new EngagementVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(document.getId());
        }
        if(!fieldsToEscape.contains("associationId")) {
            this.expandSecondLevelFields(document, vo, "associationId");
        }
        if(!fieldsToEscape.contains("event")) {
            vo.setEvent(document.getEvent());
        }
        if(!fieldsToEscape.contains("date")) {
            this.expandSecondLevelFields(document, vo, "date");
        }
        if(!fieldsToEscape.contains("time")) {
            this.expandSecondLevelFields(document, vo, "time");
        }
        if(!fieldsToEscape.contains("timestamp")) {
            this.expandSecondLevelFields(document, vo, "timestamp");
        }
        super.expandAuditFields(document, vo);
        log.debug("Converted {} to {} ", document, vo);
        return vo;
    }

    private void expandSecondLevelFields(EngagementDocument document, EngagementVo vo, String fieldName) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                if(!fieldsToEscape.contains("associationId") && fieldName.compareTo("associationId") == 0) {
                    Callable<AssociationVo> recipeDocument2VoConversion = () -> {
                        AssociationVo recipeVo = associationService.retrieveDetailsById(document.getAssociationId(), Optional.of(TOABCascadeLevel.ZERO));
                        return recipeVo;
                    };
                    String tName = "associationDocument2VoConversion";
                    ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory("associationDocument2VoConversion-"));
                    Future<AssociationVo> associationDocument2VoConversionResult = executorService.submit(recipeDocument2VoConversion);
                    try {
                        AssociationVo associationVo = associationDocument2VoConversionResult.get();
                        vo.setAssociation(associationVo);
                        log.debug("Retrieved {} for associationId: {}", associationVo, document.getAssociationId());
                    } catch (InterruptedException | ExecutionException e) {
                        String msg = "Unable to perform " + tName;
                        log.error(msg, e);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, msg, new Object[] { tName + " failure: " + e.getMessage() });
                    }
                }
                if(!fieldsToEscape.contains("timestamp") && fieldName.compareTo("timestamp") == 0) {
                    LocalDateTime timestamp = LocalDateTime.of(document.getDate(), document.getTime());
                    vo.setTimestamp(timestamp);
                }
                break;
            default:
                vo.setAssociationId(document.getAssociationId());
                vo.setDate(document.getDate());
                vo.setTime(document.getTime());
                log.debug("only first level cascaded for engagement over associationId, tableId and accountId");
                break;
        }
    }

}