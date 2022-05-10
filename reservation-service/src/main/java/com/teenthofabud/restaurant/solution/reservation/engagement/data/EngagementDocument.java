package com.teenthofabud.restaurant.solution.reservation.engagement.data;

import com.teenthofabud.core.common.data.document.TOABBaseDocument;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

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
}
