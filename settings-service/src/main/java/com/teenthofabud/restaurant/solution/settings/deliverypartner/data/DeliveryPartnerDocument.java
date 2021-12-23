package com.teenthofabud.restaurant.solution.settings.deliverypartner.data;

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
@Document("deliveryPartner")
public class DeliveryPartnerDocument extends TOABBaseDocument implements Comparable<DeliveryPartnerDocument> {

    @Id
    private String id;
    @Indexed
    private String name;
    private String description;

    @Override
    public int compareTo(DeliveryPartnerDocument o) {
        return this.getId().compareTo(o.getId());
    }
}
