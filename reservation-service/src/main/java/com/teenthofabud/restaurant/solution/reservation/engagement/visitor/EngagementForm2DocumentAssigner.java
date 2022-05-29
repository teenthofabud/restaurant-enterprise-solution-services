package com.teenthofabud.restaurant.solution.reservation.engagement.visitor;

import com.teenthofabud.restaurant.solution.reservation.engagement.data.DeliveryEngagementDocument;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.DineInEngagementDocument;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.EngagementForm;
import com.teenthofabud.restaurant.solution.reservation.engagement.data.TakeAwayEngagementDocument;

import java.util.List;

public class EngagementForm2DocumentAssigner {

    private EngagementForm form;
    private List<String> fieldsToEscape;

    public EngagementForm2DocumentAssigner(EngagementForm form, List<String> fieldsToEscape) {
        this.form = form;
        this.fieldsToEscape = fieldsToEscape;
    }

    public void assign(DineInEngagementDocument document) {
        if(!fieldsToEscape.contains("tableId")) {
            document.setTableId(form.getTableId());
        }
        if(!fieldsToEscape.contains("noOfPersons")) {
            document.setNoOfPersons(form.getNoOfPersons());
        }
    }

    public void assign(TakeAwayEngagementDocument document) {
        if(!fieldsToEscape.contains("instructions")) {
            document.setInstructions(form.getInstructions());
        }
    }

    public void assign(DeliveryEngagementDocument document) {
        if(!fieldsToEscape.contains("extRef")) {
            document.setExtRef(form.getExtRef());
        }
    }

}
