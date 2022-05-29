package com.teenthofabud.restaurant.solution.reservation.engagement.data;

import com.teenthofabud.core.common.data.document.TOABBaseDocument;
import com.teenthofabud.restaurant.solution.reservation.engagement.visitor.EngagementDto2DocumentAssigner;
import com.teenthofabud.restaurant.solution.reservation.engagement.visitor.EngagementForm2DocumentAssigner;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Document("engagement")
public abstract class EngagementDocument extends TOABBaseDocument implements Comparable<EngagementDocument> {

    @Id
    private String id;
    @Indexed
    private String bookingId;
    private String tokenNumber;

    @Override
    public int compareTo(EngagementDocument o) {
        return this.getId().compareTo(o.getId());
    }

    public abstract void assign(EngagementForm2DocumentAssigner assigner);

    public abstract Integer assign(EngagementDto2DocumentAssigner assigner);
}
