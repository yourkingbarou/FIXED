library(readxl)
data_bphmigas <- read_excel(file.choose())
view(data_bphmigas)

# lib
library(dplyr)
library(tidyr)

# EKSEKUSI MASTER
data_bphmigas_final <- data_bphmigas %>%
  
  mutate(Tahun_Bersih = as.numeric(gsub("[^0-9]", "", Tahun))) %>%
  
  rename(
    Provinsi = ProvinsiJBT,
    Kabupaten = `Kabupaten/KotaJBT`
  ) %>%
  
  select(Provinsi, Kabupaten, Tahun_Bersih, 
         matches("^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)(JBT|AKR|JBKP)$")) %>%
  
  pivot_longer(
    cols = -c(Provinsi, Kabupaten, Tahun_Bersih), 
    names_to = c("Bulan", ".value"), 
    names_pattern = "([A-Za-z]{3})(JBT|AKR|JBKP)" 
  ) %>%
  
  mutate(Bulan = factor(Bulan, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%

  arrange(Provinsi, Kabupaten, Tahun_Bersih, Bulan) %>%
  
  
  group_by(Kabupaten) %>%
  mutate(
    
    JBT = ifelse(is.na(JBT) | JBT == 0, mean(JBT[JBT > 0], na.rm = TRUE), JBT),
    JBKP = ifelse(is.na(JBKP) | JBKP == 0, mean(JBKP[JBKP > 0], na.rm = TRUE), JBKP),
    

    AKR = ifelse(is.na(AKR) | AKR == 0, 0, AKR)
  ) %>%
  ungroup() %>%
  
  # =============================================================================
  ##### INI COMMAND KALO DATA 0 NYA MAU DIJADIIN 1 #####
# ==========================================================================
  mutate(
    JBT = ifelse(is.na(JBT) | is.nan(JBT), 0, JBT),
    JBKP = ifelse(is.na(JBKP) | is.nan(JBKP), 0, JBKP),
    AKR = ifelse(is.na(AKR) | is.nan(AKR), 0, AKR)
  )

View(data_bphmigas_final)
library(writexl)
alamat_simpan_master <- "C:/Users/ASUS/OneDrive/Documents/COLLEGE/ARC 6 SIBUK/Data_BPHMigas_Master_Final_Fixed.xlsx"
write_xlsx(data_bphmigas_final, path = alamat_simpan_master)
print("udah kesave")

# Panggil pasukan visualisasi
library(ggplot2)
library(dplyr)
library(tidyr)

# ==============================================================================
# GRAFIK 1: TREN KONSUMSI NASIONAL PER BULAN (JBT vs JBKP vs AKR)
# ==============================================================================

# rekap data nasional per bulan
data_tren <- data_bphmigas_final %>%
  group_by(Bulan) %>%
  summarise(
    JBT_Pertamina = sum(JBT, na.rm = TRUE),
    JBKP_Pertamina = sum(JBKP, na.rm = TRUE),
    AKR_Non_Pertamina = sum(AKR, na.rm = TRUE)
  ) %>%
  # Lebur lagi khusus untuk kebutuhan plotting (biar bisa beda warna otomatis)
  pivot_longer(cols = -Bulan, names_to = "Jenis_BBM", values_to = "Total_Volume")

# Eksekusi Plot Tren
grafik_tren <- ggplot(data_tren, aes(x = Bulan, y = Total_Volume, color = Jenis_BBM, group = Jenis_BBM)) +
  geom_line(linewidth = 1.2) + # Garis tren
  geom_point(size = 3) +       # Titik di setiap bulan
  theme_minimal() +
  labs(
    title = "Tren Pergerakan Volume BBM Subsidi Nasional",
    subtitle = "Perbandingan JBT, JBKP, dan Penyalur Non-Pertamina (AKR)",
    x = "Bulan",
    y = "Total Realisasi Volume (KL)",
    color = "Jenis Penyaluran"
  ) +
  theme(legend.position = "bottom")

# Tampilkan Grafik 1
print(grafik_tren)

#BANTU RAPIIIINNNN
library(scales) # Panggil pembantu format angka

grafik_tren_rapi <- ggplot(data_tren, aes(x = Bulan, y = Total_Volume, color = Jenis_BBM, group = Jenis_BBM)) +
  geom_line(linewidth = 1.2) + 
  geom_point(size = 3) +       
  theme_minimal() +
  labs(
    title = "Tren Pergerakan Volume BBM Subsidi Nasional",
    subtitle = "Perbandingan JBT, JBKP, dan Penyalur Non-Pertamina (AKR)",
    x = "Bulan",
    y = "Total Realisasi Volume (KL)",
    color = "Jenis Penyaluran"
  ) +
  theme(legend.position = "bottom") +
  # INI MANTRA TAMBAHANNYA: Mengubah 1.5e+07 jadi 15,000,000
  scale_y_continuous(labels = scales::comma)

print(grafik_tren_rapi)


# ==============================================================================
# GRAFIK 2: TOP 10 PROVINSI PENYEDOT JBT (SOLAR) TERBESAR
# ==============================================================================

# Siapkan data total per provinsi
data_provinsi <- data_bphmigas_final %>%
  group_by(Provinsi) %>%
  summarise(Total_JBT_Setahun = sum(JBT, na.rm = TRUE)) %>%
  # Urutkan dari yang terbesar, lalu ambil 10 teratas saja
  arrange(desc(Total_JBT_Setahun)) %>%
  slice(1:10)

# Eksekusi Plot Bar Chart
grafik_bar <- ggplot(data_provinsi, aes(x = reorder(Provinsi, Total_JBT_Setahun), y = Total_JBT_Setahun, fill = Provinsi)) +
  geom_bar(stat = "identity") +
  coord_flip() + # Putar 90 derajat biar nama provinsi terbaca jelas
  theme_minimal() +
  labs(
    title = "Top 10 Provinsi dengan Serapan JBT (Solar) Tertinggi",
    subtitle = "Akumulasi Sepanjang Tahun",
    x = "Provinsi",
    y = "Total Realisasi Volume (KL)"
  ) +
  theme(legend.position = "none") # Sembunyikan legenda warna karena tidak perlu

# Tampilkan Grafik 2 (Kamu mungkin perlu klik panah 'Back' di tab Plots untuk lihat Grafik 1 lagi)
print(grafik_bar)

# ==============================================================================
# PERBAIKAN GRAFIK: TOP 10 PROVINSI JBT GABUNGAN (PERTAMINA + AKR)
# ==============================================================================

# Panggil pasukan visualisasi dan format angka
library(ggplot2)
library(dplyr)
library(scales) # Wajib dipanggil untuk format angka comma

# 1. Siapkan data gabungan
data_provinsi_gabungan <- data_bphmigas_final %>%
  # Langkah Kunci: Jumlahkan JBT Pertamina (JBT) dan JBT AKR (AKR) per baris
  mutate(Total_JBT_Gabungan_Row = JBT + AKR) %>%
  
  # Group by provinsi
  group_by(Provinsi) %>%
  
  # Summarise dengan menjumlahkan kolom total gabungan tadi
  summarise(Total_JBT_Gabungan_Tahun = sum(Total_JBT_Gabungan_Row, na.rm = TRUE)) %>%
  
  # Urutkan dari yang terbesar, lalu ambil 10 teratas
  arrange(desc(Total_JBT_Gabungan_Tahun)) %>%
  slice(1:10)

# 2. Ekseuksi Plot Bar Chart Gabungan
grafik_bar_gabungan <- ggplot(data_provinsi_gabungan, 
                              aes(x = reorder(Provinsi, Total_JBT_Gabungan_Tahun), 
                                  y = Total_JBT_Gabungan_Tahun, 
                                  fill = Provinsi)) +
  
  # Geom bar dengan stat = 'identity' karena kita sudah punya nilainya
  geom_bar(stat = "identity", width = 0.8) +
  
  # Putar 90 derajat biar nama provinsi terbaca jelas
  coord_flip() + 
  
  # Tema minimalis yang terlihat profesional
  theme_minimal() +
  
  # Pengaturan label dan judul yang lengkap
  labs(
    title = "Top 10 Provinsi dengan Serapan JBT (Solar) Gabungan Tertinggi",
    subtitle = "Akumulasi JBT Pertamina & JBT PT AKR Sepanjang Tahun",
    x = "Provinsi",
    y = "Total Realisasi Volume Gabungan (KL)"
  ) +
  
  # Pengaturan kosmetik (font dan posisi legenda)
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 11, face = "bold"),
    legend.position = "none" # Sembunyikan legenda warna
  ) +
  
  # JURUS SAKTI: Mengubah format angka scientific (1e+07) jadi koma (10,000,000)
  scale_y_continuous(labels = comma) +
  
  # Opsional: Gunakan palette warna yang bagus (misal: 'magma' dari viridis)
  scale_fill_viridis_d(option = "magma", direction = -1)

# Tampilkan Grafik Bar Gabungan yang sudah valid!
print(grafik_bar_gabungan)


# =======================================================================================
library(ggplot2)
library(dplyr)
library(scales)

# 1. Siapkan data agregasi per Provinsi dan Bulan
# Kita pakai Total_JBT (Pertamina + AKR) biar komprehensif
data_heatmap <- data_bphmigas_final %>%
  mutate(Total_JBT_Valid = JBT + AKR) %>%
  group_by(Provinsi, Bulan) %>%
  summarise(Total_Konsumsi = sum(Total_JBT_Valid, na.rm = TRUE), .groups = 'drop')

# 2. Eksekusi Heatmap
grafik_heatmap <- ggplot(data_heatmap, aes(x = Bulan, y = reorder(Provinsi, Total_Konsumsi), fill = Total_Konsumsi)) +
  geom_tile(color = "white", linewidth = 0.5) + # Membuat kotak-kotak dengan garis batas putih
  
  # Mewarnai kotak: dari kuning terang (rendah) ke ungu/hitam pekat (tinggi)
  scale_fill_viridis_c(option = "magma", direction = -1, labels = comma) +
  
  theme_minimal() +
  labs(
    title = "Heatmap Konsumsi JBT (Solar) per Provinsi",
    subtitle = "Mendeteksi Lonjakan Konsumsi Bulanan Sepanjang Tahun",
    x = "Bulan",
    y = "Provinsi",
    fill = "Total Volume (KL)"
  ) +
  theme(
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    panel.grid = element_blank() # Hilangkan garis background biar bersih
  )

# Tampilkan!
print(grafik_heatmap)

# ==============================================================================
# UJI STATISTIK INFERENSIAL: KORELASI & T-TEST
# ==============================================================================

# 1. UJI KORELASI PEARSON (JBT vs JBKP)
print("--- HASIL UJI KORELASI (JBT vs JBKP) ---")
uji_korelasi <- cor.test(data_bphmigas_final$JBT, data_bphmigas_final$JBKP, method = "pearson")
print(uji_korelasi)

cat("\n\n") # Bikin jarak enter di layar biar rapi

# 2. UJI PAIRED T-TEST (Beda Rata-rata JBT vs JBKP)
print("--- HASIL UJI PAIRED T-TEST (JBT vs JBKP) ---")
uji_ttest <- t.test(data_bphmigas_final$JBT, data_bphmigas_final$JBKP, paired = TRUE)
print(uji_ttest)

#===============================================================================
# Pastikan kacamata visualisasi dan format angka sudah dipakai
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# ==============================================================================
# GRAFIK 3: TREN KONSUMSI NASIONAL PER TAHUN
# ==============================================================================

# 1. Siapkan rekap data nasional per TAHUN
data_tren_tahun <- data_bphmigas_final %>%
  group_by(Tahun_Bersih) %>%
  summarise(
    JBT_Pertamina = sum(JBT, na.rm = TRUE),
    JBKP_Pertamina = sum(JBKP, na.rm = TRUE),
    AKR_Non_Pertamina = sum(AKR, na.rm = TRUE)
  ) %>%
  # Lebur memanjang agar bisa dibedakan warnanya di grafik
  pivot_longer(cols = -Tahun_Bersih, names_to = "Jenis_BBM", values_to = "Total_Volume")

# 2. Eksekusi Plot Tren Tahunan
grafik_tren_tahun <- ggplot(data_tren_tahun, aes(x = as.factor(Tahun_Bersih), y = Total_Volume, color = Jenis_BBM, group = Jenis_BBM)) +
  geom_line(linewidth = 1.2) + # Garis tren
  geom_point(size = 4) +       # Titik di setiap tahun (dibuat agak besar)
  theme_minimal() +
  labs(
    title = "Tren Pergerakan Volume BBM Subsidi Nasional per Tahun",
    subtitle = "Total Akumulasi JBT, JBKP, dan Penyalur Non-Pertamina (AKR)",
    x = "Tahun",
    y = "Total Realisasi Volume (KL)",
    color = "Jenis Penyaluran"
  ) +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  # Mengubah angka scientific jadi angka koma yang rapi
  scale_y_continuous(labels = scales::comma)

# Tampilkan grafiknya!
print(grafik_tren_tahun)