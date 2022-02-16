package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.bcdm.foodtraceability.configuration.BlogAction;
import com.bcdm.foodtraceability.entity.Management;
import com.bcdm.foodtraceability.entity.ModifyPassword;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.ManagementMapper;
import com.bcdm.foodtraceability.service.ManagementService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

import static com.bcdm.foodtraceability.common.Constants.*;
import static com.bcdm.foodtraceability.common.CreateMD5.Md5encode;
import static com.bcdm.foodtraceability.common.CreateUUID.getUUID;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 管理员服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Service
public class ManagementServiceImpl extends ServiceImpl<ManagementMapper, Management> implements ManagementService {

    @Override
    public Management login(Management management) throws Exception {
        QueryWrapper<Management> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("login_id", management.getLoginId());
        Management selectUser = getOne(queryWrapper);
        if (!(null == selectUser)) {
            if (Md5encode(management.getPassword() + selectUser.getSalt()).equals(selectUser.getPassword())) {
                return selectUser;
            }
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, LOGIN_FAIL);
    }

    @Override
    public Management register(Management management) throws Exception {
        QueryWrapper<Management> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("login_id", management.getLoginId());
        if (0 == count(queryWrapper)) {
            management.setSalt(getUUID());
            String password = management.getPassword();
            String stringBuffer = management.getPassword() + management.getSalt();
            management.setPassword(Md5encode(stringBuffer));
            LocalDateTime now = LocalDateTime.now();
            management.setManagerStatus(USER_STATUS_UNLOCK);
            management.setManagerLevel(MANAGEMENT_NORMAL_LEVEL);
            management.setCreateTime(now);
            management.setUpdateTime(now);
            save(management);
            management.setPassword(password);
            if (!save(management)) {
                throw new ServiceBusinessException(HTTP_RETURN_FAIL, REGISTER_FAIL);
            }
            management.setPassword(password);
            return login(management);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, REGISTER_FAIL);
    }

    @Override
    public Management modifyPassword(ModifyPassword userLoginInfo) throws Exception {
        Management targetUser = new Management();
        BeanUtils.copyProperties(userLoginInfo, targetUser);
        targetUser = login(targetUser);
        String stringBuffer = userLoginInfo.getNewPassword() + targetUser.getSalt();
        targetUser.setPassword(Md5encode(stringBuffer));
        return ModifyManagement(targetUser, MODIFY_PASSWORD_FAIL);
    }

    @Override
    public Management modifyUserInfo(Management management) throws Exception {
        return ModifyManagement(management, MODIFY_USERINFO_FAIL);
    }

    @Override
    public Boolean checkManager(String managementId) throws Exception {
        if (null != getById(managementId)) {
            return true;
        }
        BlogAction.logger.error("登录者ID：" + managementId + "  " + ERROR_FOR_GET_MANAGER);
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, ERROR_FOR_GET_MANAGER);
    }

    /**
     * 修改管理员信息
     *
     * @param management         查询出来的管理员
     * @param modifyUserinfoFail 更改管理员信息失败
     * @return 修改后的管理员信息
     * @throws ServiceBusinessException 修改信息失败
     */
    private Management ModifyManagement(Management management, String modifyUserinfoFail) throws ServiceBusinessException {
        UpdateWrapper<Management> updateWrapper = new UpdateWrapper<>();
        updateWrapper
                .eq("manager_id", management.getManagerId())
                .eq("update_time", management.getUpdateTime());
        management.setUpdateTime(LocalDateTime.now());
        if (update(management, updateWrapper)) {
            return management;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, modifyUserinfoFail);
    }
}
