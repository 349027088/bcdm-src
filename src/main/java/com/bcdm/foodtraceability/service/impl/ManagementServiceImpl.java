package com.bcdm.foodtraceability.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.bcdm.foodtraceability.entity.Management;
import com.bcdm.foodtraceability.exception.ServiceBusinessException;
import com.bcdm.foodtraceability.mapper.ManagementMapper;
import com.bcdm.foodtraceability.service.ManagementService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

import static com.bcdm.foodtraceability.common.Constants.*;
import static com.bcdm.foodtraceability.common.CreateMD5.Md5encode;
import static com.bcdm.foodtraceability.common.CreateUUID.getUUID;
import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_FAIL;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@Service
@Slf4j
public class ManagementServiceImpl extends ServiceImpl<ManagementMapper, Management> implements ManagementService {

    @Override
    public Management login(Management management) throws Exception {
        log.info(management.getLoginId() + "-------登录");
        QueryWrapper<Management> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("login_id", management.getLoginId());
        Management selectManagement = getOne(queryWrapper);
        if (!(null == selectManagement)) {
            StringBuilder stringBuilder = new StringBuilder();
            stringBuilder.append(management.getPassword());
            stringBuilder.append(selectManagement.getSalt());
            if (Md5encode(stringBuilder.toString()).equals(selectManagement.getPassword())) {
                return selectManagement;
            }
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, "账号密码错误");
    }

    @Override
    public Management register(Management management) throws Exception {
        log.info(management.getLoginId() + "-------注册");
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
            return login(management);
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, "当前账号已被使用");
    }

    @Override
    public Management modifyPassword(Management management, String newPassword) throws Exception {
        log.info(management.getLoginId() + "-------修改密码");
        Management targetUser = login(management);
        String stringBuffer = newPassword + targetUser.getSalt();
        targetUser.setPassword(Md5encode(stringBuffer));
        UpdateWrapper<Management> updateWrapper = new UpdateWrapper<>();
        updateWrapper.eq("manager_id", targetUser.getManagerId());
        updateWrapper.eq("update_time", targetUser.getUpdateTime());
        LocalDateTime now = LocalDateTime.now();
        management.setUpdateTime(now);
        if (update(targetUser, updateWrapper)) {
            return targetUser;
        }
        throw new ServiceBusinessException(HTTP_RETURN_FAIL, "密码修改失败");
    }

    @Override
    public Management modifyUserInfo(Management management) throws Exception {
        log.info(management.getLoginId() + "-------修改用户信息");
        Management targetUser = login(management);
        UpdateWrapper<Management> updateWrapper = new UpdateWrapper<>();
        updateWrapper.eq("manager_id", targetUser.getManagerId());
        updateWrapper.eq("update_time", targetUser.getUpdateTime());
        LocalDateTime now = LocalDateTime.now();
        management.setUpdateTime(now);
        return update(management, updateWrapper) ? management : targetUser;
    }

    @Override
    public boolean lockUser(Management management) throws Exception {
        log.info(management.getLoginId() + "-------锁定用户");
        management.setManagerStatus(USER_STATUS_LOCK);
        UpdateWrapper<Management> updateWrapper = forStatusUpdate(management);
        LocalDateTime now = LocalDateTime.now();
        management.setUpdateTime(now);
        return update(management, updateWrapper);
    }

    @Override
    public boolean unLockUser(Management management) throws Exception {
        log.info(management.getLoginId() + "-------解锁用户");
        management.setManagerStatus(USER_STATUS_UNLOCK);
        UpdateWrapper<Management> updateWrapper = forStatusUpdate(management);
        management.setUpdateTime(LocalDateTime.now());
        return update(management, updateWrapper);
    }

    /**
     * 管理员锁定解锁用共通处理
     * @param management 需要被操作的管理员
     * @return 生成的SQL操作
     */
    private UpdateWrapper<Management> forStatusUpdate(Management management){
        UpdateWrapper<Management> updateWrapper = new UpdateWrapper<>();
        updateWrapper.eq("manager_id", management.getManagerId());
        updateWrapper.set("manager_status", management.getManagerStatus());
        updateWrapper.eq("update_time", management.getUpdateTime());
        return updateWrapper;
    }
}
