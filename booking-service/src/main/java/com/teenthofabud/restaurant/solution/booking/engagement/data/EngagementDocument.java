package com.teenthofabud.restaurant.solution.booking.engagement.data;

import com.teenthofabud.core.common.data.document.TOABBaseDocument;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;

import java.time.LocalDate;
import java.time.LocalTime;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Document("engagement")
public class EngagementDocument extends TOABBaseDocument implements Comparable<EngagementDocument> {

    @Id
    private String id;
    @Indexed
    private String associationId;
    @Indexed
    private EngagementEvent event;
    @Indexed
    private LocalDate date;
    private LocalTime time;

    @Override
    public int compareTo(EngagementDocument o) {
        return this.getId().compareTo(o.getId());
    }
}
