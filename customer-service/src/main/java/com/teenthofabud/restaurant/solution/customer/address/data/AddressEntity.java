package com.teenthofabud.restaurant.solution.customer.address.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountEntity;
import lombok.*;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Entity
@Table(name = "customer_address")
@EntityListeners(AuditingEntityListener.class)
public class AddressEntity extends TOABBaseEntity implements Comparable<AddressEntity> {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    private String name;
    @Column(name = "address_line_1")
    private String addressLine1;
    @Column(name = "address_line_2")
    private String addressLine2;
    @Column(name = "city_id")
    private String cityId;
    @Column(name = "state_id")
    private String stateId;
    private String pincode;
    @Column(name = "country_id")
    private String countryId;
    @ManyToOne(fetch = FetchType.LAZY,cascade = CascadeType.MERGE)
    @JoinColumn(name = "customer_account_id")
    private AccountEntity account;

    @Override
    public int compareTo(AddressEntity o) {
        return this.getId().compareTo(o.getId());
    }
}
