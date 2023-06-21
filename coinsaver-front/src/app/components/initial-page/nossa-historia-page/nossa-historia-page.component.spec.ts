import { ComponentFixture, TestBed } from '@angular/core/testing';

import { NossaHistoriaPageComponent } from './nossa-historia-page.component';

describe('NossaHistoriaPageComponent', () => {
  let component: NossaHistoriaPageComponent;
  let fixture: ComponentFixture<NossaHistoriaPageComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ NossaHistoriaPageComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(NossaHistoriaPageComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
