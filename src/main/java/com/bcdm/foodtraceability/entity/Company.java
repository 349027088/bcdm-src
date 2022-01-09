package com.bcdm.foodtraceability.entity;

import java.time.LocalDateTime;
import java.io.Serializable;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * <p>
 * 企业
 * </p>
 *
 * @author 王
 * @since 2022-01-09
 */
@Data
  @EqualsAndHashCode(callSuper = false)
    public class Company implements Serializable {

    private static final long serialVersionUID=1L;

      /**
     * 企业ID
     */
        private Integer companyId;

      /**
     * 企业状态
     */
      private Integer companyStatus;

      /**
     * 企业级别
     */
      private Integer companyLevel;

      /**
     * 企业名称
     */
      private String companyName;

      /**
     * 企业地址
     */
      private String companyAddress;

      /**
     * 企业信息
     */
      private String companyInfo;

      /**
     * 营业执照
     */
      private String businessLicense;

      /**
     * 卫生许可
     */
      private String healthPermit;

    private LocalDateTime createTime;

    private LocalDateTime updateTime;


}
