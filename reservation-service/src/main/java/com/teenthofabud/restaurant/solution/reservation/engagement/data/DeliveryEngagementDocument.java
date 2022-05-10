package com.teenthofabud.restaurant.solution.reservation.engagement.data;

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

}
