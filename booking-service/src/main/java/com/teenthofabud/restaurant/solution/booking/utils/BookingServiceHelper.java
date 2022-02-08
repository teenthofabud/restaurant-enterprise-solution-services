package com.teenthofabud.restaurant.solution.booking.utils;

import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.booking.engagement.data.EngagementVo;
import com.teenthofabud.restaurant.solution.booking.association.converter.AssociationDocument2VoConverter;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationDocument;
import com.teenthofabud.restaurant.solution.booking.association.data.AssociationVo;
import com.teenthofabud.restaurant.solution.booking.engagement.converter.EngagementDocument2VoConverter;
import com.teenthofabud.restaurant.solution.booking.engagement.data.EngagementDocument;
import com.teenthofabud.restaurant.solution.booking.experience.converter.ExperienceDocument2VoConverter;
import com.teenthofabud.restaurant.solution.booking.experience.data.ExperienceDocument;
import com.teenthofabud.restaurant.solution.booking.experience.data.ExperienceVo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.LinkedList;
import java.util.List;

@Slf4j
@Component
public class BookingServiceHelper {

    private ExperienceDocument2VoConverter experienceDocument2VoConverter;
    private AssociationDocument2VoConverter associationDocument2VoConverter;
    private EngagementDocument2VoConverter engagementDocument2VoConverter;


    @Autowired
    public void setAssociationDocument2VoConverter(AssociationDocument2VoConverter associationDocument2VoConverter) {
        this.associationDocument2VoConverter = associationDocument2VoConverter;
    }

    @Autowired
    public void setExperienceDocument2VoConverter(ExperienceDocument2VoConverter experienceDocument2VoConverter) {
        this.experienceDocument2VoConverter = experienceDocument2VoConverter;
    }

    @Autowired
    public void setEngagementDocument2VoConverter(EngagementDocument2VoConverter engagementDocument2VoConverter) {
        this.engagementDocument2VoConverter = engagementDocument2VoConverter;
    }

    public List<ExperienceVo> experienceDocument2DetailedVo(List<? extends ExperienceDocument> experienceDocumentList) throws TOABSystemException {
        List<ExperienceVo> experienceDetailsList = new LinkedList<>();
        if(experienceDocumentList != null && !experienceDocumentList.isEmpty()) {
            for(ExperienceDocument document : experienceDocumentList) {
                ExperienceVo vo = this.experienceDocument2DetailedVo(document);
                experienceDetailsList.add(vo);
            }
        }
        return experienceDetailsList;
    }

    public ExperienceVo experienceDocument2DetailedVo(ExperienceDocument experienceDocument) throws TOABSystemException {
        if(experienceDocument != null) {
            ExperienceVo vo = experienceDocument2VoConverter.convert(experienceDocument);
            log.debug("Converting {} to {}", experienceDocument, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "experience document is null" });
    }

    public List<AssociationVo> associationDocument2DetailedVo(List<? extends AssociationDocument> associationDocumentList) {
        List<AssociationVo> associationDetailsList = new LinkedList<>();
        if(associationDocumentList != null && !associationDocumentList.isEmpty()) {
            for(AssociationDocument document : associationDocumentList) {
                AssociationVo vo = this.associationDocument2DetailedVo(document);
                associationDetailsList.add(vo);
            }
        }
        return associationDetailsList;
    }

    public AssociationVo associationDocument2DetailedVo(AssociationDocument associationDocument) {
        if(associationDocument != null) {
            AssociationVo vo = associationDocument2VoConverter.convert(associationDocument);
            log.debug("Converting {} to {}", associationDocument, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "association document is null" });
    }

    public List<EngagementVo> engagementDocument2DetailedVo(List<? extends EngagementDocument> engagementDocumentList) {
        List<EngagementVo> engagementDetailsList = new LinkedList<>();
        if(engagementDocumentList != null && !engagementDocumentList.isEmpty()) {
            for(EngagementDocument document : engagementDocumentList) {
                EngagementVo vo = this.engagementDocument2DetailedVo(document);
                engagementDetailsList.add(vo);
            }
        }
        return engagementDetailsList;
    }

    public EngagementVo engagementDocument2DetailedVo(EngagementDocument engagementDocument) {
        if(engagementDocument != null) {
            EngagementVo vo = engagementDocument2VoConverter.convert(engagementDocument);
            log.debug("Converting {} to {}", engagementDocument, vo);
            return vo;
        }
        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, new Object[] { "engagement document is null" });
    }
}
