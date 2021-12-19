package com.teenthofabud.restaurant.solution.settings.paymentmethod.data;

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
@Document("paymentMethod")
public class PaymentMethodDocument extends TOABBaseDocument implements Comparable<PaymentMethodDocument> {

    @Id
    private String id;
    @Indexed
    private String name;
    private String description;

    @Override
    public int compareTo(PaymentMethodDocument o) {
        return this.getId().compareTo(o.getId());
    }
}
