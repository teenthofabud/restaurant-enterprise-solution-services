package com.teenthofabud.restaurant.solution.reservation.engagement.data;

import com.teenthofabud.restaurant.solution.reservation.engagement.visitor.EngagementDto2DocumentAssigner;
import com.teenthofabud.restaurant.solution.reservation.engagement.visitor.EngagementForm2DocumentAssigner;
import lombok.*;
import org.springframework.data.annotation.TypeAlias;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Document("engagement")
@TypeAlias("delivery")
public class DeliveryEngagementDocument extends EngagementDocument {

    @Indexed
    private String extRef;

    public void assign(EngagementForm2DocumentAssigner assigner) {
        assigner.assign(this);
    }

    @Override
    public Integer assign(EngagementDto2DocumentAssigner assigner) {
        return assigner.assign(this);
    }

}
