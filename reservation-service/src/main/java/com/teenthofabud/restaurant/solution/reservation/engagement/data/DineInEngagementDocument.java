package com.teenthofabud.restaurant.solution.reservation.engagement.data;

import com.teenthofabud.core.common.data.document.TOABBaseDocument;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.TypeAlias;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Document("engagement")
@TypeAlias("dineIn")
public class DineInEngagementDocument extends EngagementDocument {

    private Integer noOfPerson;
    @Indexed
    private String tableId;

}
