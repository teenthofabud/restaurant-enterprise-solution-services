package com.teenthofabud.restaurant.solution.customer.account.data;

import com.teenthofabud.core.common.data.entity.TOABBaseEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;
import java.time.LocalDate;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Entity
@Table(name = "customer_account")
@EntityListeners(AuditingEntityListener.class)
public class AccountEntity extends TOABBaseEntity implements Comparable<AccountEntity> {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    @Column(name = "first_name")
    private String firstName;
    @Column(name = "last_name")
    private String lastName;
    @Column(name = "gender_id")
    private String genderId;
    @Column(name = "date_of_birth")
    private LocalDate dateOfBirth;
    @Column(name = "phone_number")
    private String phoneNumber;
    @Column(name = "country_code")
    private String countryCode;
    @Column(name = "email_id")
    private String emailId;


    @Override
    public int compareTo(AccountEntity o) {
        return this.getId().compareTo(o.getId());
    }
}
