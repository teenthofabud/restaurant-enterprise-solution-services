package com.teenthofabud.restaurant.solution.engagement.booking.data;

import com.teenthofabud.core.common.data.document.TOABBaseDocument;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;

import java.time.LocalDateTime;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Document("booking")
public class BookingDocument extends TOABBaseDocument implements Comparable<BookingDocument> {

    @Id
    private String id;
    @Indexed
    private String categoryId;
    @Indexed
    private LocalDateTime timestamp;
    @Indexed
    private String accountId;

    @Override
    public int compareTo(BookingDocument o) {
        return this.getId().compareTo(o.getId());
    }
}
