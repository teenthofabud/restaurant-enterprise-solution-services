package com.teenthofabud.restaurant.solution.settings.charge.data;

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
@Document("charge")
public class ChargeDocument extends TOABBaseDocument implements Comparable<ChargeDocument> {

    @Id
    private String id;
    @Indexed
    private String name;
    private String description;
    private Double rate;

    @Override
    public int compareTo(ChargeDocument o) {
        return this.getId().compareTo(o.getId());
    }
}
