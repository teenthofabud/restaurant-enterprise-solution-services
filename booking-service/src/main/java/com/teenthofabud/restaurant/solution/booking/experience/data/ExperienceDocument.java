package com.teenthofabud.restaurant.solution.booking.experience.data;

import com.teenthofabud.core.common.data.document.TOABBaseDocument;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Document("experience")
public class ExperienceDocument extends TOABBaseDocument implements Comparable<ExperienceDocument> {

    @Id
    private String id;
    @Indexed
    private String name;
    private String description;

    @Override
    public int compareTo(ExperienceDocument o) {
        return this.getId().compareTo(o.getId());
    }
}
