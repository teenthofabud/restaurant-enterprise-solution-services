package com.teenthofabud.restaurant.solution.booking.association.data;

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
@Document("association")
public class AssociationDocument extends TOABBaseDocument implements Comparable<AssociationDocument> {

    @Id
    private String id;
    @Indexed
    private String experienceId;
    @Indexed
    private String tableId;
    @Indexed
    private String accountId;
    //private LocalDateTime endedOn;

    @Override
    public int compareTo(AssociationDocument o) {
        return this.getId().compareTo(o.getId());
    }
}
